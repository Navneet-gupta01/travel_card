package com.navneetgupta.domain

trait ZonesRepository[F[_]] {
  def getZonesByStationCode(stationCode: String): F[List[Int]]
}

trait CardsRepository[F[_]] {
  def createCard(amount: Option[Double]): F[OysterCard]

  def getCard(cardNumber: Long): F[Option[OysterCard]]

  def updateCard(card: OysterCard): F[Option[OysterCard]]
}
