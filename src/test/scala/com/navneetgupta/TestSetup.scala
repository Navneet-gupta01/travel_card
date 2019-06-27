package com.navneetgupta

import cats.Monad
import com.navneetgupta.domain._
import com.navneetgupta.infra.{InMemoryCardsRepositoryInterpreter, InMemoryZonesRepositoryInterpreter}

trait TestSetup {
  class TestSetup[F[_]: Monad: RandomGenerator]() {
    val cardRepo : CardsRepository[F] = InMemoryCardsRepositoryInterpreter[F]
    val zonesRepo: ZonesRepository[F] = InMemoryZonesRepositoryInterpreter[F]

    val cardServices: CardServices[F] = CardServices[F](cardRepo, zonesRepo)
  }
}
