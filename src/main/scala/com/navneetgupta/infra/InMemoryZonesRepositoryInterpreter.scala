package com.navneetgupta.infra

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import com.navneetgupta.oyster_tagless.{Station, ZonesRepository}


class InMemoryZonesRepositoryInterpreter[F[_] : Applicative] extends ZonesRepository[F] {
  private val zonesCache: Map[String, Station] = Map(
    "HOL" -> Station("HOL", "Holborn", NonEmptyList(1, Nil)),
    "EAR" -> Station("EAR", "Earl’s Court", NonEmptyList(1, 2 :: Nil)),
    "HAM" -> Station("HAM", "Hammersmith", NonEmptyList(2, Nil)),
    "WIM" -> Station("WIM", "Wimbledon", NonEmptyList(3, Nil))
  )

  override def getZonesByStationCode(stationCode: String): F[List[Int]] =
    zonesCache.get(stationCode.toUpperCase)
      .map(a => a.zones.sorted.toList)
      .toList
      .flatten
      .pure[F]
}

object InMemoryZonesRepositoryInterpreter {
  def apply[F[_] : Applicative]: InMemoryZonesRepositoryInterpreter[F] = new InMemoryZonesRepositoryInterpreter[F]()
}
