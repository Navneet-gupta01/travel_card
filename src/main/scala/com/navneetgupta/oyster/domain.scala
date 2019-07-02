package com.navneetgupta.oyster

import java.util.{Date, UUID}

import cats.data.NonEmptyList

sealed trait JourneyType extends Product with Serializable

case object BusJourney extends JourneyType

case object TubeJourney extends JourneyType

object Direction extends Enumeration {
  type Direction = Value
  val CHECK_IN, CHECK_OUT = Value
}

final case class Barrier(stationCode: String, journeyType: JourneyType, direction: Direction.Value, crossedAt: Date, fare: Double)

object Barrier {
  def apply(stationCode: String, journeyType: JourneyType, direction: Direction.Value, crossedAt: Date): Barrier =
    Barrier(stationCode, journeyType, direction, crossedAt, 0.0)

  def apply(stationCode: String, journeyType: JourneyType, direction: Direction.Value): Barrier =
    Barrier(stationCode, journeyType, direction, new Date(), 0.0)
}

//case class Journey(from: Barrier, to: Option[Barrier], date: Date, cost: Double)
final case class OysterCard[A](number: A, balance: Double, lastBarrier: Option[Barrier] = None) {
  def update(card: OysterCard[A]): OysterCard[A] =
    this.copy(
      number = this.number,
      balance = card.balance,
      lastBarrier = card.lastBarrier
    )
}

final case class Station(stationCode: String, stationName: String, zones: NonEmptyList[Int])

sealed trait ValidationError extends Exception with Product with Serializable {
  def msg: String
}

case object MinBalanceError extends ValidationError {
  override def msg: String = "Amount Insufficient"
}

case object CardDoesNotExistError extends ValidationError{
  override def msg: String = "Invalid Card Number"
}

case object BalanceUpdateError extends ValidationError{
  override def msg: String = "Error While Updating Balance"
}

case object CreateJourneyError extends ValidationError{
  override def msg: String = "Error While Creating Journey"
}

case object BarrierNotCheckedIN extends ValidationError{
  override def msg: String = "Barrier Not Checked IN"
}
case object InvalidOptionSelected extends ValidationError{
  override def msg: String = "Invalid Option Selected"
}

case class InvalidInputParams(msg: String) extends ValidationError