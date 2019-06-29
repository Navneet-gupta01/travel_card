package com.navneetgupta.domain


import java.util.Date

import cats.{Monad}
import cats.data.EitherT
import cats.implicits._
import CardServices._

class CardServices[F[_]: RandomGenerator](cardsRepository: CardsRepository[F],
                                          zonesRepository: ZonesRepository[F]) {


  def createCard(amount: Option[Double]) = cardsRepository.createCard(amount)

  def updateBalance(amountToAdd: Double, cardNumber: Long)(
    implicit M: Monad[F]): F[Either[ValidationError, OysterCard]] = {
    (for {
      card <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.getCard(cardNumber),
        CardDoesNotExistError)
      updatedCard <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.updateCard(
          card.copy(balance = card.balance + amountToAdd)),
        BalanceUpdateError)
    } yield updatedCard).value
  }

  def getBalance(cardNumber: Long)(
    implicit M: Monad[F]): F[Either[ValidationError, Double]] =
    (for {
      cardBalance <- EitherT.fromOptionF[F, ValidationError, Double](
        cardsRepository.getCard(cardNumber).fmap(_.map(_.balance)),
        CardDoesNotExistError)
    } yield cardBalance).value

  def createJourney(barrier: Barrier, cardNumber: Long)(implicit M: Monad[F]) =
  {
    (for {
      card <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.getCard(cardNumber),
        CardDoesNotExistError)
      crossedBarrier <- barrier.journeyType match {
        case BusJourney  => busJourney(barrier, card)
        case TubeJourney => tubeJourney(barrier, card)
      }
      updateCard <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.updateCard(
          card.copy(balance = (card.balance - crossedBarrier.fare),
            lastBarrier = crossedBarrier.copy(crossedAt = new Date()).some,
          )),
        CreateJourneyError)
    } yield crossedBarrier).value
  }



  private def tubeJourney(barrier: Barrier, card: OysterCard)(
    implicit M: Monad[F]): EitherT[F, ValidationError, Barrier] =
    for {
      minBalanceValidation        <- EitherT{minBalanceValidation(barrier, card)}
      updatedBarrierWihtFare      <- EitherT{processTubeJourney(minBalanceValidation, card)}
    } yield updatedBarrierWihtFare

  private def busJourney(barrier: Barrier, card: OysterCard)(
    implicit M: Monad[F]): EitherT[F, ValidationError, Barrier] =
    for {
      minBalanceValidation    <- EitherT {minBalanceValidation(barrier, card)}
      updatedBarrierWithFare  <- EitherT{calculateBusFare(minBalanceValidation, card)}
    } yield updatedBarrierWithFare

  private def minBalanceValidation(barrier: Barrier, card: OysterCard)(
    implicit M: Monad[F]): F[Either[ValidationError, Barrier]] = {
    (CardServices.MIN_BALANCE_FOR_CHECK_IN
      .get(barrier.journeyType)
      .filter(_ <= card.balance)
      .fold({
        Either.left[ValidationError, Barrier](MinBalanceError)})(v => {
        Right(barrier)}))
      .pure[F]
  }

  // If User is trying to continuously IN multiple time, he should not be charger again.
  private def isContinuousCheckIN(lastCheckIn: Date, currentCheckIN: Date): Boolean = {
    (currentCheckIN.getTime-lastCheckIn.getTime) < 5000
  }


  private def processTubeJourney(
                                  barrier: Barrier,
                                  card: OysterCard)(implicit M: Monad[F]): F[Either[ValidationError, Barrier]] = {
    card.lastBarrier.fold(
      if(barrier.direction == Direction.CHECK_IN)
        Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_TUBE_JOURNEY_FARE)).pure[F]
      else Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[F])(lastBarrier => {
      lastBarrier.journeyType match {
        case BusJourney =>  if(barrier.direction == Direction.CHECK_IN)
          Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_TUBE_JOURNEY_FARE)).pure[F]
        else
          Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[F]
        case TubeJourney =>
          (lastBarrier.direction, barrier.direction) match {
            case (Direction.CHECK_OUT, Direction.CHECK_OUT) =>
              Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[F]
            case (Direction.CHECK_IN, Direction.CHECK_OUT) =>
              calculateMinTubeFare(barrier, lastBarrier).map(Either.right(_))
            case _ =>
              Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_TUBE_JOURNEY_FARE)).pure[F]
          }
      }
    })
  }

  private def calculateBusFare(barrier: Barrier, card: OysterCard)(implicit M: Monad[F]): F[Either[ValidationError,Barrier]] =
    (barrier.direction, card.lastBarrier) match {
      case (Direction.CHECK_OUT, Some(lastbarrier)) if (lastbarrier.journeyType == BusJourney && lastbarrier.direction == Direction.CHECK_IN) =>
        // already deducted while checkIn donot deduct again
        Either.right[ValidationError, Barrier](barrier).pure[F]
      case _ =>
        // In any other case dedcut the fare.
        Either.right[ValidationError, Barrier](barrier.copy(fare = MAX_BUS_JOURNEY_FARE)).pure[F]
    }

  private def calculateMinTubeFare(to: Barrier, from: Barrier)(implicit M: Monad[F]): F[Barrier] = {
    for {
      fromZones <- zonesRepository.getZonesByStationCode(from.stationCode)
      toZones <- zonesRepository.getZonesByStationCode(to.stationCode)
    } yield {
      // refund the excess fare charged
      val minFare = minFareCalc(fromZones,toZones)
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

  private def calculateTubeFare(crossedZones: List[Int], minNumberOfZonesCrossed: Long) : Double = {
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


  def apply[F[_]: RandomGenerator](cardsRepository: CardsRepository[F],
                                   zonesRepository: ZonesRepository[F]): CardServices[F] = new CardServices(cardsRepository, zonesRepository)
}
