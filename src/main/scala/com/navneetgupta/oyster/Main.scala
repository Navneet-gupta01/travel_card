package com.navneetgupta.oyster


import com.navneetgupta.oyster.CardRepository.InMemoryCardRepository
import com.navneetgupta.oyster.ZonesRepository.InMemoryZonesRepository
import zio.{App, Ref, TaskR, ZIO}
import zio.console._

object Main extends App {
  type AppEnvironment = Console with CardRepository with ZonesRepository
  type CardTask[A] = TaskR[AppEnvironment, A]
  type CreatedEnv = CardRepository with ZonesRepository

  override def run(args: List[String]): ZIO[Main.Environment, Nothing, Int] = (for {
    _ <- putStrLn("Starting Application")
    store    <- Ref.make(Map[Long, OysterCard[Long]]())
    counter <- Ref.make(0L)
    zioAppEnv = ZIO.runtime[AppEnvironment].flatMap{ implicit rts => {
      val cardServices = new CardServices[AppEnvironment]
      Programs[AppEnvironment](cardServices).loop
    }}
    server <- zioAppEnv.provideSome[Environment](base => {
      new Console with CardRepository with ZonesRepository {
        override val console: Console.Service[Any] = base.console

        override def cardRepository: CardRepository.Service[Any] = InMemoryCardRepository(store, counter)

        override val zonesRepository: ZonesRepository.Service[Any] = InMemoryZonesRepository
      }
    })
  } yield server).foldM(err => putStrLn(s"Execution failed with: ${err}") *> ZIO.succeed(1), _ => ZIO.succeed(0))

}
final case class Programs[R <: Console with CardRepository with ZonesRepository](cardServices: CardServices[R]) {
  val inputs =
    """
Please select Options from below Menu
  [1] Create a Card
  [2] Recharge Card
  [3] Get Balance
  [4] Proceed With Bus Journey
  [5] Proceed With Tube Journey
  [6] Exit
    """.stripMargin

  type CardTask[A] = ZIO[R,ValidationError, A]

  def createCardOption = for {
    amount <- readData("Please enter the amount default[0]")
    amountValidate <- ZIO.succeed(Validation.validateDouble(amount))
    card <- cardServices.createCard(amountValidate)
    _ <- putStrLn(s"Card Created Successfully, Your Card Number is: ${card.number} and balance is: ${card.balance}")
  } yield ()


  def getBalanceOption =
    for {
      cardNumber <- readData("Please enter the card Number")
      cardNumberValidated <- ZIO.fromOption(Validation.validateLong(cardNumber)).mapError(_ => InvalidInputParams("Invalid Card Number"))
      balance <- cardServices.getBalance(cardNumberValidated)
      _ <- putStrLn(s"Your Card Balance is $balance")
    } yield ()

  def rechargeCardOption =
    for {
      cardNumber <- readData("Please enter the card Number")
      amount <- readData("Please Enter amount to rcharge the card")
      tuple <- ZIO.fromOption(Validation.validateTuple2((cardNumber, amount))).mapError(_ => InvalidInputParams("Invalid Card Number/Amount"))
      updatedCard <- cardServices.updateBalance(tuple._2, tuple._1)
      _ <- putStrLn(s"Your Updated Balance is ${updatedCard.balance}")
    } yield ()

  def processBusJourneyOption =
    for {
      stationCode <- readData("Please Enter the stationCode")
      direction <- readData("Please Enter the Direction For Inward Journey(IN)/ for OutWard Journey(OUT)")
      card <- readData("Please Enter Card Number")
      tuple <-  ZIO.fromOption(Validation.validateTuple3((stationCode, direction, card))).mapError(_ => InvalidInputParams("Invalid inputs"))
      crossedBarrier <- cardServices.createJourney(Barrier(tuple._3, BusJourney, tuple._2), tuple._1)
      _ <-  putStrLn(s"You are allowed to cross through: ${crossedBarrier}")
    } yield ()

  def processTubeJourneyOption =
    for {
      stationCode <- readData("Please Enter the stationCode")
      direction <- readData("Please Enter the Direction For Inward Journey(IN)/ for OutWard Journey(OUT)")
      card <- readData("Please Enter Card Number")
      tuple <-  ZIO.fromOption(Validation.validateTuple3((stationCode, direction, card))).mapError(_ => InvalidInputParams("Invalid inputs"))
      crossedBarrier <- cardServices.createJourney(Barrier(tuple._3, TubeJourney, tuple._2), tuple._1)
      _ <-  putStrLn(s"You are allowed to cross through: ${crossedBarrier}")
    } yield ()


  def loop: CardTask[Unit] =
    for {
      selectedOption <- readData(inputs)
      option <- ZIO.fromOption[Long](Validation.validateLong(selectedOption)).mapError(_ => InvalidOptionSelected)
      _ <- if(option > 0 && option < 6) processOption(option).catchAll(err => putStrLn(s"${err.detailMessage}")) *> loop
      else if(option == 6) putStrLn("Exiting Application!!")
      else putStrLn("Invalid Option Selected. Exiting Application!!")
    } yield ()


  def processOption(option: Long): CardTask[Unit] = {
    option match {
      case 1 => createCardOption
      case 2 => rechargeCardOption
      case 3 => getBalanceOption
      case 4 => processBusJourneyOption
      case 5 => processTubeJourneyOption
    }
  }

  private def readData(msg: String): ZIO[R, ValidationError, String] = putStrLn(msg) *> getStrLn.mapError(_ => InvalidInputParams(msg))

  object Validation {
    import cats.implicits._
    def getDirection(str: String): Option[Direction.Value] = str match {
      case "IN" => Some(Direction.CHECK_IN)
      case "OUT" => Some(Direction.CHECK_OUT)
      case _ => None
    }

    def validateDouble(num: String): Option[Double] = scala.util.Try(num.toDouble).toOption

    def validateLong(num: String): Option[Long] = scala.util.Try(num.toLong).toOption

    def validateTuple2(tuple: (String, String)): Option[(Long, Double)] = (scala.util.Try(tuple._1.toLong).toOption, scala.util.Try(tuple._2.toDouble).toOption).bisequence

    def validateTuple3(tuple: (String, String, String)): Option[(Long, Direction.Value, String)] =
      (scala.util.Try(tuple._3.toLong).toOption, getDirection(tuple._2)).bisequence.map(a => (a._1, a._2, tuple._1))
  }
}

object Programs {
  def apply[R <: Console with CardRepository with ZonesRepository](cardServices: CardServices[R]): Programs[R] = new Programs[R](cardServices)
}