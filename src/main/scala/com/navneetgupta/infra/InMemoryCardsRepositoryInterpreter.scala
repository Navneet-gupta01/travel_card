package com.navneetgupta.infra

import cats.Monad
import com.navneetgupta.oyster_tagless.{CardsRepository, OysterCard, RandomGenerator}
import cats.implicits._

import scala.collection.concurrent.TrieMap


class InMemoryCardsRepositoryInterpreter[F[_] : Monad : RandomGenerator] extends CardsRepository[F] {
  private val cache = new TrieMap[Long, OysterCard]

  private val defaultAmount = 0.0

  def nextLong() = RandomGenerator[F].getNextLong

  override def createCard(amount: Option[Double]): F[OysterCard] =
    for {
      id <- nextLong()
      cardToSave = OysterCard(id, amount.getOrElse(defaultAmount))
      _ = cache.put(cardToSave.number, cardToSave)
    } yield cardToSave

  override def getCard(cardNumber: Long): F[Option[OysterCard]] = cache.get(cardNumber).pure[F]

  override def updateCard(card: OysterCard): F[Option[OysterCard]] = {
    cache.put(card.number, card).pure[F] *> cache.get(card.number).pure[F]
  }

}

object InMemoryCardsRepositoryInterpreter {
  def apply[F[_] : Monad : RandomGenerator](): InMemoryCardsRepositoryInterpreter[F] = new InMemoryCardsRepositoryInterpreter[F]()
}
