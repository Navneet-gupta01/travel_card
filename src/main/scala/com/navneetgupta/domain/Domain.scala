package com.navneetgupta.domain

import java.util.Date

import cats.data.NonEmptyList


sealed trait JourneyType extends Product with Serializable
case object BusJourney extends JourneyType
case object TubeJourney extends JourneyType

object Direction extends Enumeration {
  type Direction = Value
  val CHECK_IN, CHECK_OUT = Value
}
case class Barrier(stationCode: String, journeyType: JourneyType, direction: Direction.Value, crossedAt: Date, fare: Double)

object Barrier {
  def apply(stationCode: String, journeyType: JourneyType, direction: Direction.Value,crossedAt: Date): Barrier  =
    Barrier(stationCode, journeyType, direction,crossedAt, 0.0)

  def apply(stationCode: String, journeyType: JourneyType, direction: Direction.Value): Barrier  =
    Barrier(stationCode, journeyType, direction,new Date(), 0.0)
}

//case class Journey(from: Barrier, to: Option[Barrier], date: Date, cost: Double)
case class OysterCard(number: Long, balance: Double, lastBarrier: Option[Barrier]= None)

case class Station(stationCode: String, stationName: String, zones: NonEmptyList[Int])

sealed trait ValidationError extends Product with Serializable
case object MinBalanceError extends ValidationError
case object CardDoesNotExistError extends ValidationError
case object BalanceUpdateError extends ValidationError
case object CreateJourneyError extends ValidationError
case object BarrierNotCheckedIN extends ValidationError
