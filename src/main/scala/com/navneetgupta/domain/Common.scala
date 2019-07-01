package com.navneetgupta.domain


object Common {

  trait Console[F[_]] {
    def putStrLn(str: String): F[Unit]

    def getStrLn(): F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console](): F[String] = Console[F].getStrLn()

}
