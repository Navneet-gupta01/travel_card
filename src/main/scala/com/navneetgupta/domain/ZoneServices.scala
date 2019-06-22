package com.navneetgupta.domain

class ZoneServices[F[_]](zonesRepository: ZonesRepository[F]) {
  def getZoneByStationCode(stationCode: String): F[List[Int]] =
    zonesRepository.getZonesByStationCode(stationCode)

  def getMinNumberOfZonesCrossed(fromZones: List[Int], toZones: List[Int]) : Int =
    getMinNumberOfZonesCrossed(fromZones, toZones, Integer.MAX_VALUE)

  private def getMinNumberOfZonesCrossed(fromZones: List[Int], toZones: List[Int], minZonesCrossed: Int): Int = {
    fromZones match {
      case Nil => minZonesCrossed + 1
      case head :: tail =>
        getMinNumberOfZonesCrossed(tail, toZones, toZones.foldLeft(minZonesCrossed)(
          (min, toZone) => if (toZone - head < min) toZone - head else min))
    }
  }
}

object ZoneServices {
  def apply[F[_]](zonesRepository: ZonesRepository[F]): ZoneServices[F] = new ZoneServices[F](zonesRepository)
}
