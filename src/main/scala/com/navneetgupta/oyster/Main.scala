package com.navneetgupta.oyster

import cats.Monad
import zio.clock.Clock
import zio.{App, TaskR, ZIO}
import zio.console._

object Main extends App {
  type AppEnvironment = Clock with Console with CardRepository with ZonesRepository
  type CardTask[A] = TaskR[AppEnvironment, A]

  override def run(args: List[String]): ZIO[Main.Environment, Nothing, Int] = ???
}

final case class Program[R <: Console with CardRepository with ZonesRepository](cardServices: CardServices[R]) {
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

  type CardTask[A] = TaskR[R, A]

  def createCardOption: CardTask[Unit] = for {
    amount <- readData("Please enter the amount default[0]")
    amountValidate <- ZIO.succeed(Validation.validateDouble(amount))
    card <- cardServices.createCard(amountValidate)
    _ <- putStrLn(s"Card Created Successfully, Your Card Number is: ${card.number} and balance is: ${card.balance}")
  } yield ()


  def getBalanceOption(implicit M: Monad[CardTask]): CardTask[Unit] =
    for {
      cardNumber <- readData("Please enter the card Number")
      _ <- Validation.validateLong(cardNumber).fold(
        putStrLn("Invalid Card Number."))(number =>
        cardServices.getBalance(number).flatMap(x => {
          x match {
            case Right(balance) => putStrLn(s"Your Card Balance is ${balance}")
            case Left(_) => putStrLn("Invalid Card Number")
          }
        }))
    } yield ()

  def rechargeCardOption(implicit M: Monad[CardTask]): CardTask[Unit] =
    for {
      cardNumber <- readData("Please enter the card Number")
      amount <- readData("Please Enter amount to rcharge the card")
      _ <- Validation.validateTuple2((cardNumber, amount)).fold(
        putStrLn("Invalid Card Number."))(input =>
        cardServices.updateBalance(input._2, input._1).flatMap(x => {
          x match {
            case Right(updatedCard) => putStrLn(s"Your Updated Card Balance is ${updatedCard.balance}")
            case Left(_) => putStrLn("Invalid Card Number")
          }
        }))
    } yield ()

  def processBusJourneyOption(implicit M: Monad[CardTask]): CardTask[Unit] =
    for {
      stationCode <- readData("Please Enter the stationCode")
      direction <- readData("Please Enter the Direction For Inward Journey(IN)/ for OutWard Journey(OUT)")
      card <- readData("Please Enter Card Number")
      _ <- Validation.validateTuple3((stationCode, direction, card)).fold(putStrLn("Invalid Inputs"))(input => {
        cardServices.createJourney(Barrier(input._3, BusJourney, input._2), input._1).flatMap(x =>
        {
          x match {
            case Right(crossedBarrier) => putStrLn(s"You are allowed to cross through: ${crossedBarrier}")
            case Left(error) => putStrLn(s"Unable To Create Journey error: $error")
          }
        }
        )
      })
    } yield ()

  def processTubeJourneyOption(implicit M: Monad[CardTask]): CardTask[Unit] =
    for {
      stationCode <- readData("Please Enter the stationCode")
      direction <- readData("Please Enter the Direction For Inward Journey(IN)/ for OutWard Journey(OUT)")
      card <- readData("Please Enter Card Number")
      _ <- Validation.validateTuple3((stationCode, direction, card)).fold(putStrLn("Invalid Inputs"))(input => {
        cardServices.createJourney(Barrier(input._3, TubeJourney, input._2), input._1).flatMap[Console, Throwable, Unit](x => {
          x match {
            case Right(crossedBarrier) => putStrLn(s"You are allowed to cross through: ${crossedBarrier}")
            case Left(error) => putStrLn(s"Unable To Create Journey error: $error")
          }
        })
      })
    } yield ()


  def loop(implicit M: Monad[CardTask]): CardTask[Unit] =
    for {
      selectedOption <- readData(inputs)
      resp <- Validation.validateLong(selectedOption).fold({
        loop
      })(option => {
        if (option > 0 && option < 6)
          processOption(option) *> loop
        else if (option == 6) putStrLn("Exiting Application !!")
        else putStrLn("Invalid Option Selected. Exiting Application !!")
      })
    } yield ()


  def processOption(option: Long)(implicit M: Monad[CardTask]): CardTask[Unit] = {
    option match {
      case 1 => createCardOption
      case 2 => rechargeCardOption
      case 3 => getBalanceOption
      case 4 => processBusJourneyOption
      case 5 => processTubeJourneyOption
    }
  }

  private def readData(msg: String): CardTask[String] = putStrLn(msg) *> getStrLn

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