package com.navneetgupta.domain


trait RandomGenerator[F[_]] {
  def getNextLong: F[Long]
}

object RandomGenerator {
  def apply[F[_]](implicit F: RandomGenerator[F]): RandomGenerator[F] = F
}
