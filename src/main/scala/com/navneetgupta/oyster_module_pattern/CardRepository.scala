package com.navneetgupta.oyster_module_pattern

import zio.{Ref, UIO, ZIO}

trait CardRepository extends Serializable {
  def cardRepository: CardRepository.Service[Any]
}

object CardRepository extends Serializable {

  trait Service[R] extends Serializable {
    def get(cardNumber: Long): ZIO[R, Nothing, Option[OysterCard[Long]]]

    def create(amount: Option[Double]): ZIO[R, Nothing, OysterCard[Long]]

    def update(cardNumber: Long, card: OysterCard[Long]): ZIO[R, Nothing, Option[OysterCard[Long]]]
  }

  final case class InMemoryCardRepository(ref: Ref[Map[Long, OysterCard[Long]]], counter: Ref[Long]) extends Service[Any] {
    override def get(cardNumber: Long): ZIO[Any, Nothing, Option[OysterCard[Long]]] = ref.get.map(_.get(cardNumber))

    override def create(amount: Option[Double]): ZIO[Any, Nothing, OysterCard[Long]] =
      for {
        newCounter <- counter.update(_ + 1)
        card = OysterCard(newCounter, amount.getOrElse(0.0D), None)
        _ <- ref.update(store => store + (newCounter -> card))
      } yield card

    override def update(cardNumber: Long, card: OysterCard[Long]): ZIO[Any, Nothing, Option[OysterCard[Long]]] =
      for {
        oldCard <- get(cardNumber)
        result <- oldCard.fold[UIO[Option[OysterCard[Long]]]](ZIO.succeed(None)) { x =>
          val newValue = x.update(card)
          ref.update(store => store + (newValue.number -> newValue)) *> ZIO.succeed(Some(newValue))
        }
      } yield result
  }

}