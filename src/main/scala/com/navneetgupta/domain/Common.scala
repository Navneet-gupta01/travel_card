package com.navneetgupta.domain


object Common {
  trait Console[F[_]] {
    def putStrLn(str: String): F[Unit]

    def readLn(): F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def readLn[F[_] : Console](): F[String] = Console[F].readLn()

}
