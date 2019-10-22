package com.navneetgupta

import com.navneetgupta.oyster_tagless._
import org.scalatest.{Matchers, Outcome, fixture}
import zio.{DefaultRuntime, IO}
import zio.interop.catz._

import scala.util.Random


class CardServiceSpec extends TestSetup with fixture.FunSpecLike with Matchers {

  type Task[A] = IO[Throwable, A]

  implicit val RandomGeneratorIO = new RandomGenerator[Task] {
    def getNextLong: Task[Long] = IO(scala.util.Random.nextLong())
  }
  implicit val runtime = new DefaultRuntime {}

  override type FixtureParam = TestSetup[Task]

  override def withFixture(test: OneArgTest): Outcome = test(new TestSetup[Task]())


  describe("CardServices.CreateCard") {
    it("should be able to create card") { fixture =>
      val createCardResult = fixture.cardServices.createCard(None)
      val oysterCard = runtime.unsafeRun(createCardResult)
      oysterCard.balance shouldBe 0.0D
      oysterCard.lastBarrier shouldBe None
    }
  }

  describe("CardServices.UpdateBalance") {

    it("should be able to add balance to valid card") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(30.0D, card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Right(30.0D)
    }

    it("should fail to add balance to Invalid card") { fixture =>
      val invalidCardNumber = (new Random()).nextLong()
      val getBalance = for {
        balance <- fixture.cardServices.updateBalance(35.0D, invalidCardNumber)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Left(CardDoesNotExistError)
    }

  }

  describe("CardServices.GetBalance") {

    it("should be able to get balance from valid card") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(30.0D, card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Right(30.0D)
    }

    it("should fail to add balance to Invalid card") { fixture =>
      val invalidCardNumber = (new Random()).nextLong()
      val getBalance = for {
        balance <- fixture.cardServices.updateBalance(35.0D, invalidCardNumber)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Left(CardDoesNotExistError)
    }

    it("should fail to get balance for an invalid card ") { fixture =>
      val invalidCardNumber = (new Random()).nextLong()
      val getBalance = for {
        balance <- fixture.cardServices.getBalance(invalidCardNumber)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Left(CardDoesNotExistError)
    }
  }


  describe("CardServices.createJourney") {
    it("should allow bus journey with card Balance >= 1.8D") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(1.8D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", BusJourney, Direction.CHECK_IN), card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Right(0.0D)
    }

    it("should not allow bus journey with Invalid Card") { fixture =>
      val createJourney = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(3.0D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", BusJourney, Direction.CHECK_IN), 233)
      } yield journey

      runtime.unsafeRun(createJourney) shouldBe Left(CardDoesNotExistError)
    }

    it("should not allow bus journey with card Balance < 1.8D") { fixture =>
      val createJourney = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(1.5D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", BusJourney, Direction.CHECK_IN), card.number)
      } yield journey

      runtime.unsafeRun(createJourney) shouldBe Left(MinBalanceError)
    }

    it("should allow tube journey with card Balance >= 3.2D") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(3.2D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", TubeJourney, Direction.CHECK_IN), card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Right(0.0D)
    }

    it("should not allow tube journey with Invalid Card") { fixture =>
      val createJourney = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(3.2D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", TubeJourney, Direction.CHECK_IN), 233)
      } yield journey

      runtime.unsafeRun(createJourney) shouldBe Left(CardDoesNotExistError)
    }

    it("should not allow tube journey with card Balance < 3.2D") { fixture =>
      val createJourney = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(1.5D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", TubeJourney, Direction.CHECK_IN), card.number)
      } yield journey

      runtime.unsafeRun(createJourney) shouldBe Left(MinBalanceError)
    }

    it("should dedcut maxBalance in case used forgots to checkout") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(30.0D, card.number)
        journey <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", TubeJourney, Direction.CHECK_IN), card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      runtime.unsafeRun(getBalance) shouldBe Right(26.8D)
    }
  }

  val exampleJourney =
    """
      -Tube Holborn to Earl’s Court
      -328 bus from Earl’s Court to Chelsea
      -Tube Earl’s court to Hammersmith"""


  describe(s"Should Successfully complete Provided Example Journey $exampleJourney") {

    it("should successfully complete journey") { fixture =>
      val completedJourney = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(30.0D, card.number)
        _ <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HOL", TubeJourney, Direction.CHECK_IN), card.number) // 3.2
        balance1 <- fixture.cardServices.getBalance(card.number)
        print = println(s"balance after HOL IN $balance1")
        _ <- fixture.cardServices.createJourney(oyster_tagless.Barrier("EAR", TubeJourney, Direction.CHECK_OUT), card.number) //
        balance2 <- fixture.cardServices.getBalance(card.number)
        print = println(s"balance after EAR OUT $balance2")
        _ <- fixture.cardServices.createJourney(oyster_tagless.Barrier("EAR", BusJourney, Direction.CHECK_IN), card.number)
        balance3 <- fixture.cardServices.getBalance(card.number)
        print = println(s"balance after BUS OUT $balance3")
        _ <- fixture.cardServices.createJourney(oyster_tagless.Barrier("EAR", TubeJourney, Direction.CHECK_IN), card.number)
        balance4 <- fixture.cardServices.getBalance(card.number)
        print = println(s"balance after EAR IN $balance4")
        _ <- fixture.cardServices.createJourney(oyster_tagless.Barrier("HAM", TubeJourney, Direction.CHECK_OUT), card.number)
        balance5 <- fixture.cardServices.getBalance(card.number)
        print = println(s"balance after HAM OUT $balance5")
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      runtime.unsafeRun(completedJourney) shouldBe Right(23.7D)
    }
  }
}