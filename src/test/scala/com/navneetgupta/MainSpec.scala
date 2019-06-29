package com.navneetgupta

import cats.Monad
import com.navneetgupta.domain.{Common, Programs, RandomGenerator}
import org.scalatest.{FunSpecLike, Matchers}

class MainSpec extends FunSpecLike with Matchers {

  case class TestData(input: List[String], output: List[String], nums: List[Long]) {
    def putStrLn(line: String): (TestData, Unit) =
      (copy(output = line :: output), Unit)

    def getStrLn(): (TestData, String) =
      (copy(input = input.tail), input.head)

    def getNextLong: (TestData, Long) =
      (copy(), nums.head) // Generating same number each time since. Valid for one Card number

    def showResults = output.reverse.mkString("\n")
  }

  case class TestIO[A](run: TestData => (TestData, A)) {
    self =>
    def map[B](ab: A => B): TestIO[B] = TestIO(t => self.run(t) match {
      case (t, a) => (t, ab(a))
    })

    def flatMap[B](afb: A => TestIO[B]): TestIO[B] = TestIO(t => self.run(t) match {
      case (t, a) => afb(a).run(t)
    })

    def eval(t: TestData): TestData = run(t)._1
  }

  object TestIO {
    def pure[A](a: => A): TestIO[A] = TestIO(t => (t, a))


    implicit val ConsoleTestIO = new Common.Console[TestIO] {
      def putStrLn(line: String): TestIO[Unit] = TestIO(t => t.putStrLn(line))

      def getStrLn(): TestIO[String] = TestIO(t => t.getStrLn())
    }

    implicit val RandomGeneratorIO = new RandomGenerator[TestIO] {
      def getNextLong: TestIO[Long] = TestIO(t => t.getNextLong)
    }
  }
  implicit val testIOMonad: Monad[TestIO] = new Monad[TestIO] {
    override def flatMap[A, B](opt: TestIO[A])(fn: A => TestIO[B]): TestIO[B] = opt.flatMap(fn)

    override def pure[A](opt: A): TestIO[A] = TestIO.pure(opt)

    def tailRecM[A, B](a: A)(fn: A => TestIO[Either[A, B]]): TestIO[B] = ???

  }
  def programTest : TestIO[Unit] = Programs.program[TestIO]

  val testData = TestData(
    input = "1" :: "12" :: "43" :: Nil,
    output = Nil,
    nums = 1 :: Nil
  )
  def runTest(testData: TestData) = programTest.eval(testData).showResults

  describe("Program should work") {
    it ("in creating the card and successfully exiting the application") {
      val resp =
        s"""Starting The Program
            ${Programs.inputs}
            Please enter the amount default[0]
            Card Created Successfully, Your Card Number is: 1 and balance is: 12.0
            ${Programs.inputs}
            Invalid Option Selected. Exiting Application !!""".stripMargin

      runTest(testData).replace(" ", "") shouldBe resp.replace(" ", "")
    }
    it ("in creating the card, recharge the card and successfully exiting the application") {
      val resp =
        s"""Starting The Program
            ${Programs.inputs}
            Please enter the amount default[0]
            Card Created Successfully, Your Card Number is: ${testData.nums.head} and balance is: 12.0
            ${Programs.inputs}
            Please enter the card Number
            Please Enter amount to rcharge the card
            Your Updated Card Balance is 35.0
            ${Programs.inputs}
            Invalid Option Selected. Exiting Application !!""".stripMargin

      runTest(testData.copy(
        input = List("1", "12", "2", testData.nums.head.toString, "23", "43")))
        .replace(" ", "") shouldBe resp.replace(" ", "")
    }
//    it ("in creating the card and successfully exiting the application") {
//      val resp =
//        s"""Starting The Program
//            ${Programs.inputs}
//            Please enter the amount default[0]
//            Card Created Successfully, Your Card Number is: ${testData.nums.head} and balance is: 12.0
//            ${Programs.inputs}
//            Please enter the card Number
//            Please Enter amount to rcharge the card
//            Your Updated Card Balance is 35.0
//            ${Programs.inputs}
//            Invalid Option Selected. Exiting Application !!""".stripMargin
//
//      runTest(testData).replace(" ", "") shouldBe resp.replace(" ", "")
//    }
  }
}
