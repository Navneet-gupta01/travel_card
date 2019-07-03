package com.navneetgupta
import zio.ZIO

package object oyster extends CardRepository.Service[CardRepository] with ZonesRepository.Service[ZonesRepository] {
  override def get(cardNumber: Long): ZIO[CardRepository, Nothing, Option[OysterCard[Long]]] =
    ZIO.accessM(_.cardRepository.get(cardNumber))

  override def create(amount: Option[Double]): ZIO[CardRepository, Nothing, OysterCard[Long]] =
    ZIO.accessM(_.cardRepository.create(amount))

  override def update(cardNumber: Long, card: OysterCard[Long]): ZIO[CardRepository, Nothing, Option[OysterCard[Long]]] =
    ZIO.accessM(_.cardRepository.update(cardNumber,card))

  override def getZonesByStationCode(code: String): ZIO[ZonesRepository, Nothing, List[Int]] =
    ZIO.accessM(_.zonesRepository.getZonesByStationCode(code))
}
