package com.navneetgupta.oyster_module_pattern

import cats.data.NonEmptyList
import zio.ZIO
import zio.IO

trait ZonesRepository extends Serializable {
  val zonesRepository: ZonesRepository.Service[Any]
}

object ZonesRepository extends Serializable {
  trait Service[R] extends Serializable {
    def getZonesByStationCode(code: String): ZIO[R, Nothing, List[Int]]
  }

  final case object InMemoryZonesRepository extends Service[Any] {
    private val zonesCache: Map[String, Station] = Map(
      "HOL" -> Station("HOL", "Holborn", NonEmptyList(1, Nil)),
      "EAR" -> Station("EAR", "Earl’s Court", NonEmptyList(1, 2 :: Nil)),
      "HAM" -> Station("HAM", "Hammersmith", NonEmptyList(2, Nil)),
      "WIM" -> Station("WIM", "Wimbledon", NonEmptyList(3, Nil))
    )
    override def getZonesByStationCode(code: String): ZIO[Any, Nothing, List[Int]] =
      IO.effectTotal(zonesCache.get(code).fold(List[Int]())(_.zones.toList))
  }
}
