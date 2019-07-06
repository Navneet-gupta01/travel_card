package com.navneetgupta.oyster

import java.util.Date

import zio.{TaskR, ZIO}
import cats.implicits._

final case class CardServices[R <: CardRepository with ZonesRepository]() extends Serializable {
  import CardServices._

  type CardTask[A] = ZIO[R, ValidationError, A]

  def createCard(amount: Option[Double]): CardTask[OysterCard[Long]] = create(amount)

  def updateBalance(amountToAdd: Double, cardNumber: Long): CardTask[OysterCard[Long]] =
    for {
      cardOption <- get(cardNumber)
      card <- ZIO.fromOption(cardOption).mapError(_ => CardDoesNotExistError)
      updateCard <- update(cardNumber, card.copy(balance = card.balance + amountToAdd))
      balanceUpdated <- ZIO.fromOption(updateCard).mapError(_ => BalanceUpdateError)
  } yield balanceUpdated

  def getBalance(cardNumber: Long): CardTask[Double] =
    for {
      card <- get(cardNumber)
      balance <- ZIO.fromOption(card).mapError(_ => CardDoesNotExistError).map(_.balance)
    } yield balance

  def createJourney(barrier: Barrier, cardNumber: Long): CardTask[Barrier] =
    for {
      cardOption <- get(cardNumber)
      card <- ZIO.fromOption(cardOption).mapError(_ => CardDoesNotExistError)
      crossedBarrier <- barrier.journeyType match {
        case BusJourney => busJourney(barrier, card)
        case TubeJourney => tubeJourney[Long](barrier, card)
      }
      updateCard <- update(cardNumber,
        card.copy(balance = (card.balance - crossedBarrier.fare),
          lastBarrier = crossedBarrier.copy(crossedAt = new Date()).some,
        ))
      _ <- ZIO.fromOption(updateCard).mapError(_ => CreateJourneyError)

    } yield crossedBarrier


  private def tubeJourney[A](barrier: Barrier, card: OysterCard[A]): CardTask[Barrier] =
    for {
      validatedBarrier <- minBalanceValidation(barrier, card)
      updatedBarrierWihtFare <- processTubeJourney(validatedBarrier, card)
    } yield updatedBarrierWihtFare

  private def busJourney[A](barrier: Barrier, card: OysterCard[A]): CardTask[Barrier] =
    for {
      validatedBarrier <- minBalanceValidation(barrier, card)
      updatedBarrierWithFare <- calculateBusFare(validatedBarrier, card)
    } yield updatedBarrierWithFare

  private def minBalanceValidation[A](barrier: Barrier, card: OysterCard[A]): CardTask[Barrier] =
    ZIO.fromOption(CardServices.MIN_BALANCE_FOR_CHECK_IN
      .get(barrier.journeyType)
      .filter(_ <= card.balance)).map(_ => barrier).mapError(_ => MinBalanceError)


  private def processTubeJourney[A](
                                  barrier: Barrier,
                                  card: OysterCard[A]): CardTask[Barrier] =
    card.lastBarrier match {
      case Some(lastBarrier) =>
        lastBarrier.journeyType match {
          case BusJourney => if (barrier.direction == Direction.CHECK_IN)
            ZIO.succeed(barrier.copy(fare = MAX_TUBE_JOURNEY_FARE))
          else
            TaskR.fail(BarrierNotCheckedIN)
          case TubeJourney =>
            (lastBarrier.direction, barrier.direction) match {
              case (Direction.CHECK_OUT, Direction.CHECK_OUT) =>
                TaskR.fail(BarrierNotCheckedIN)
              case (Direction.CHECK_IN, Direction.CHECK_OUT) =>
                calculateMinTubeFare(barrier, lastBarrier)
              case _ =>
                ZIO.succeed(barrier.copy(fare = MAX_TUBE_JOURNEY_FARE))
            }
        }
      case None =>
        if (barrier.direction == Direction.CHECK_IN)
          ZIO.succeed(barrier.copy(fare = MAX_TUBE_JOURNEY_FARE))
        else
          ZIO.fail(BarrierNotCheckedIN)
    }

  private def calculateBusFare[A](barrier: Barrier, card: OysterCard[A]): CardTask[Barrier] =
    (barrier.direction, card.lastBarrier) match {
      case (Direction.CHECK_OUT, Some(lastbarrier)) if (lastbarrier.journeyType == BusJourney && lastbarrier.direction == Direction.CHECK_IN) =>
        // already deducted while checkIn donot deduct again
        ZIO.succeed(barrier)
      case _ =>
        // In any other case deduct the fare.
        ZIO.succeed(barrier.copy(fare = MAX_BUS_JOURNEY_FARE))
    }

  private def calculateMinTubeFare(to: Barrier, from: Barrier): CardTask[Barrier] = {
    val r = for {
      fromZones <- getZonesByStationCode(from.stationCode)
      toZones <- getZonesByStationCode(to.stationCode)
    } yield {
      // refund the excess fare charged
      val minFare = minFareCalc(fromZones, toZones)
      val barrierC = to.copy(fare = minFare - MAX_TUBE_JOURNEY_FARE)
      barrierC
    }
    r
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