package com.navneetgupta.domain

import cats.Monad
import cats.implicits._
import com.navneetgupta.infra.{InMemoryCardsRepositoryInterpreter, InMemoryZonesRepositoryInterpreter}
import zio._
import zio.interop.catz._
import Common._


object Main extends App {

  type Task[A] = IO[Throwable, A]

  implicit val ConsoleIO = new Common.Console[Task] {
    def putStrLn(line: String): Task[Unit] = ZIO.effectTotal(println(s"$line \n"))

    def getStrLn(): Task[String] = ZIO.effect(scala.io.StdIn.readLine)
  }

  implicit val RandomGeneratorIO = new RandomGenerator[Task] {
    override def getNextLong: Task[Long] = IO(Math.abs(scala.util.Random.nextLong()))
  }

  override def run(args: List[String]) =  Programs.program.fold(_ => 1, _ => 0)
}

object Programs {

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

  def createCardOption[F[_]: Common.Console: Monad ](cardServices: CardServices[F]): F[Unit] = for {
    amount <- readData("Please enter the amount default[0]")
    amountValidate <- Validation.validateDouble(amount).pure[F]
    card <- cardServices.createCard(amountValidate)
    _ <- putStrLn(s"Card Created Successfully, Your Card Number is: ${card.number} and balance is: ${card.balance}")
  } yield ()

  def getBalanceOption[F[_]: Common.Console: Monad](cardServices: CardServices[F]): F[Unit] =
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

  def rechargeCardOption[F[_]: Common.Console: Monad](cardServices: CardServices[F]): F[Unit] =
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

  def processBusJourneyOption[F[_]: Common.Console: Monad](cardServices: CardServices[F]): F[Unit] =
    for {
      stationCode <-  readData("Please Enter the stationCode")
      direction <- readData("Please Enter the Direction For Inward Journey(IN)/ for OutWard Journey(OUT)")
      card <- readData("Please Enter Card Number")
      _ <- Validation.validateTuple3((stationCode, direction, card)).fold(putStrLn("Invalid Inputs"))(input => {
        cardServices.createJourney(Barrier(input._3, BusJourney, input._2), input._1).flatMap(x => {
          x match {
            case Right(crossedBarrier) => putStrLn(s"You are allowed to crosss through: ${crossedBarrier}")
            case Left(error) => putStrLn(s"Unable To Create Journey error: $error")
          }
        })
      })
    } yield ()

  def processTubeJourneyOption[F[_]: Common.Console: Monad](cardServices: CardServices[F]): F[Unit] =
    for {
      stationCode <- readData("Please Enter the stationCode")
      direction <- readData("Please Enter the Direction For Inward Journey(IN)/ for OutWard Journey(OUT)")
      card <- readData("Please Enter Card Number")
      _ <- Validation.validateTuple3((stationCode, direction, card)).fold(putStrLn("Invalid Inputs"))(input => {
        cardServices.createJourney(Barrier(input._3, TubeJourney, input._2), input._1).flatMap(x => {
          x match {
            case Right(crossedBarrier) => putStrLn(s"You are allowed to cross through: ${crossedBarrier}")
            case Left(error) => putStrLn(s"Unable To Create Journey error: $error")
          }
        })
      })
    } yield ()


  def invalidOption[F[_]: Common.Console: Monad]: F[Unit] = putStrLn("Invalid Option selected")


  def loop[F[_]: Common.Console: Monad](cardServices: CardServices[F]): F[Unit] =
    for {
      selectedOption <- readData(inputs)
      resp <- Validation.validateLong(selectedOption).fold({
        loop[F](cardServices)
      })(option => {
        if(option > 0 && option < 6)
          processOption[F](option,cardServices) *> loop[F](cardServices)
        else if (option == 6) putStrLn("Exiting Application !!")
        else putStrLn("Invalid Option Selected. Exiting Application !!")
      })
    } yield ()


  def processOption[F[_]: Common.Console: Monad](option: Long, cardServices: CardServices[F]): F[Unit] = {
    option match {
      case 1 => createCardOption(cardServices)
      case 2 => rechargeCardOption(cardServices)
      case 3 => getBalanceOption(cardServices)
      case 4 => processBusJourneyOption(cardServices)
      case 5 => processTubeJourneyOption(cardServices)
    }
  }

  private def readData[F[_]: Common.Console: Monad](msg: String): F[String] = putStrLn(msg) *> getStrLn()

  object Validation {
    def getDirection(str: String): Option[Direction.Value] = str match {
      case "IN" => Direction.CHECK_IN.some
      case "OUT" => Direction.CHECK_OUT.some
      case _ => None
    }
    def validateDouble(num: String): Option[Double] = scala.util.Try(num.toDouble).toOption
    def validateLong(num: String): Option[Long] = scala.util.Try(num.toLong).toOption
    def validateTuple2(tuple: (String , String)): Option[(Long, Double)] =  (scala.util.Try(tuple._1.toLong).toOption, scala.util.Try(tuple._2.toDouble).toOption).bisequence
    def validateTuple3(tuple: (String , String, String)): Option[(Long, Direction.Value, String)] =  {
      (scala.util.Try(tuple._3.toLong).toOption, getDirection(tuple._2)).bisequence.map(a => (a._1, a._2, tuple._1))
    }
  }



  def program[F[_]:Common.Console: Monad: RandomGenerator]: F[Unit] =
    for {
      _ <- putStrLn("Starting The Program")
      cardRepositories = InMemoryCardsRepositoryInterpreter[F]
      zoneRepositories = InMemoryZonesRepositoryInterpreter[F]
      cardServices = CardServices[F](cardRepositories, zoneRepositories)
      resp <- loop[F](cardServices)
    } yield resp
}
