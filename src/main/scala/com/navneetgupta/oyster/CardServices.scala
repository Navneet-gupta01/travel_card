package com.navneetgupta.oyster

import java.util.Date

import cats.Monad
import cats.data.EitherT
import zio.{TaskR, ZIO}
import cats.implicits._

final case class CardServices[R <: CardRepository with ZonesRepository]() extends Serializable {
  import CardServices._

  type CardTask[A] = TaskR[R, A]

  def createCard(amount: Option[Double]): CardTask[OysterCard[Long]] = create(amount)

  def updateBalanceService(amountToAdd: Double, cardNumber: Long)(implicit M: Monad[CardTask]): CardTask[Either[ValidationError,OysterCard[Long]]] = {
    (for {
      card <- EitherT.fromOptionF[CardTask, ValidationError, OysterCard[Long]](
        get(cardNumber),
        CardDoesNotExistError)
      updatedCard <- EitherT.fromOptionF[CardTask, ValidationError, OysterCard[Long]](
        update(cardNumber,
          card.copy(balance = card.balance + amountToAdd)),
        BalanceUpdateError)
    } yield updatedCard).value
  }

  def getBalanceService(cardNumber: Long)(
    implicit M: Monad[CardTask]): CardTask[Either[ValidationError, Double]] =
    (for {
      cardBalance <- EitherT.fromOptionF[CardTask, ValidationError, OysterCard[Long]](
        get(cardNumber),
        CardDoesNotExistError)
    } yield cardBalance.balance).value

  def createJourneyService(barrier: Barrier, cardNumber: Long)(implicit M: Monad[CardTask]) = {
    (for {
      card <- EitherT.fromOptionF[CardTask, ValidationError, OysterCard[Long]](
        get(cardNumber),
        CardDoesNotExistError)
      crossedBarrier <- barrier.journeyType match {
        case BusJourney => busJourney(barrier, card)
        case TubeJourney => tubeJourney[Long](barrier, card)
      }
      _ <- EitherT.fromOptionF[CardTask, ValidationError, OysterCard[Long]](
        update(cardNumber,
          card.copy(balance = (card.balance - crossedBarrier.fare),
            lastBarrier = crossedBarrier.copy(crossedAt = new Date()).some,
          )),
        CreateJourneyError)
    } yield crossedBarrier).value
  }

  private def tubeJourney[A](barrier: Barrier, card: OysterCard[A])(
    implicit M: Monad[CardTask]): EitherT[CardTask, ValidationError, Barrier] =
    for {
      minBalanceValidation <- EitherT {
        minBalanceValidation(barrier, card)
      }
      updatedBarrierWihtFare <- EitherT {
        processTubeJourney(minBalanceValidation, card)
      }
    } yield updatedBarrierWihtFare

  private def busJourney[A](barrier: Barrier, card: OysterCard[A])(
    implicit M: Monad[CardTask]): EitherT[CardTask, ValidationError, Barrier] =
    for {
      minBalanceValidation <- EitherT {
        minBalanceValidation(barrier, card)
      }
      updatedBarrierWithFare <- EitherT {
        calculateBusFare(minBalanceValidation, card)
      }
    } yield updatedBarrierWithFare

  private def minBalanceValidation[A](barrier: Barrier, card: OysterCard[A])(
    implicit M: Monad[CardTask]): CardTask[Either[ValidationError, Barrier]] = {
    (CardServices.MIN_BALANCE_FOR_CHECK_IN
      .get(barrier.journeyType)
      .filter(_ <= card.balance)
      .fold({
        Either.left[ValidationError, Barrier](MinBalanceError)
      })(v => {
        Right(barrier)
      }))
      .pure[CardTask]
  }

  // If User is trying to continuously IN multiple time, he should not be charger again.
  private def isContinuousCheckIN(lastCheckIn: Date, currentCheckIN: Date): Boolean = {
    (currentCheckIN.getTime - lastCheckIn.getTime) < 5000
  }


  private def processTubeJourney[A](
                                  barrier: Barrier,
                                  card: OysterCard[A])(implicit M: Monad[CardTask]): CardTask[Either[ValidationError, Barrier]] = {
    card.lastBarrier.fold(
      if (barrier.direction == Direction.CHECK_IN)
        Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_TUBE_JOURNEY_FARE)).pure[CardTask]
      else Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[CardTask])(lastBarrier => {
      lastBarrier.journeyType match {
        case BusJourney => if (barrier.direction == Direction.CHECK_IN)
          Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_TUBE_JOURNEY_FARE)).pure[CardTask]
        else
          Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[CardTask]
        case TubeJourney =>
          (lastBarrier.direction, barrier.direction) match {
            case (Direction.CHECK_OUT, Direction.CHECK_OUT) =>
              Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[CardTask]
            case (Direction.CHECK_IN, Direction.CHECK_OUT) =>
              calculateMinTubeFare(barrier, lastBarrier).map(Either.right(_))
            case _ =>
              Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_TUBE_JOURNEY_FARE)).pure[CardTask]
          }
      }
    })
  }

  private def calculateBusFare[A](barrier: Barrier, card: OysterCard[A])(implicit M: Monad[CardTask]): CardTask[Either[ValidationError, Barrier]] =
    (barrier.direction, card.lastBarrier) match {
      case (Direction.CHECK_OUT, Some(lastbarrier)) if (lastbarrier.journeyType == BusJourney && lastbarrier.direction == Direction.CHECK_IN) =>
        // already deducted while checkIn donot deduct again
        Either.right[ValidationError, Barrier](barrier).pure[CardTask]
      case _ =>
        // In any other case deduct the fare.
        Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_BUS_JOURNEY_FARE)).pure[CardTask]
    }

  private def calculateMinTubeFare(to: Barrier, from: Barrier)(implicit M: Monad[CardTask]): CardTask[Barrier] = {
    for {
      fromZones <- getZonesByStationCode(from.stationCode)
      toZones <- getZonesByStationCode(to.stationCode)
    } yield {
      // refund the excess fare charged
      val minFare = minFareCalc(fromZones, toZones)
      val barrierC = to.copy(fare = minFare - MAX_TUBE_JOURNEY_FARE)
      barrierC
    }
  }

  private def minFareCalc(fromZones: List[Int], toZones: List[Int]): Double = {
    fromZones.foldLeft(MAX_TUBE_JOURNEY_FARE)((min, fromZone) =>
      toZones.foldLeft(min)((localMin, toZone) => {
        val fareC = calculateTubeFare(List(fromZone, toZone), toZone - fromZone + 1)
        if (localMin > fareC) fareC else localMin
      }))
  }

  private def calculateTubeFare(crossedZones: List[Int], minNumberOfZonesCrossed: Long): Double = {
    val cost = (crossedZones.contains(1), minNumberOfZonesCrossed) match {
      case (true, 1) => 2.5D
      case (true, 2) => 3.0D
      case (false, 1) => 2.0D
      case (false, 2) => 2.25D
      case _ => MAX_TUBE_JOURNEY_FARE
    }
    cost
  }

}

object CardServices {
  val MAX_BUS_JOURNEY_FARE = 1.8D
  val MAX_TUBE_JOURNEY_FARE = 3.2D
  val MIN_BALANCE_FOR_CHECK_IN = Map(BusJourney -> MAX_BUS_JOURNEY_FARE, TubeJourney -> MAX_TUBE_JOURNEY_FARE)
}