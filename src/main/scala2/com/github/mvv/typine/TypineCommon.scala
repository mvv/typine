package com.github.mvv.typine

import com.github.mvv.typine.impl.TypineMacros

import scala.annotation.implicitNotFound
import scala.language.higherKinds
import scala.language.experimental.macros

trait TypineCommon {
  @implicitNotFound("could not prove that types ${A} and ${B} are different")
  sealed trait !:=[A, B] {
    implicit def flip: B !:= A
  }
  object !:= {
    private val singleton = new !:=[Any, Any] {
      implicit override def flip: Any !:= Any = this
      override def toString: String = "!:="
    }
    def unsafeMake[A, B]: A !:= B = singleton.asInstanceOf[A !:= B]
    implicit def derive[A, B]: A !:= B = macro TypineMacros.deriveUnequal[A, B]
  }

  def substituteCoBounded[L, U >: L, From >: L <: U, To >: L <: U, F[+_ >: L <: U]](f: F[From])(
      implicit witness: From <:< To): F[To] =
    f.asInstanceOf[F[To]]

  def substituteContraBounded[L, U >: L, From >: L <: U, To >: L <: U, F[-_ >: L <: U]](f: F[To])(
      implicit witness: From <:< To): F[From] =
    f.asInstanceOf[F[From]]

  def substituteBounded[L, U >: L, From >: L <: U, To >: L <: U, F[_ >: L <: U]](f: F[From])(
      implicit witness: From =:= To): F[To] =
    f.asInstanceOf[F[To]]

  def liftCoBounded[L, U >: L, From >: L <: U, To >: L <: U, F[+_ >: L <: U]](
      implicit witness: From <:< To): F[From] <:< F[To] = {
    type G[+T >: L <: U] = F[From] <:< F[T]
    substituteCoBounded[L, U, From, To, G](implicitly[G[From]])
  }

  def liftContraBounded[L, U >: L, From >: L <: U, To >: L <: U, F[-_ >: L <: U]](
      implicit witness: From <:< To): F[To] <:< F[From] = {
    type G[-T >: L <: U] = F[To] <:< F[T]
    substituteContraBounded[L, U, From, To, G](implicitly[G[To]])
  }

  def liftBounded[L, U >: L, From >: L <: U, To >: L <: U, F[_ >: L <: U]](
      implicit witness: From =:= To): F[From] =:= F[To] = {
    type G[T >: L <: U] = F[From] =:= F[T]
    substituteBounded[L, U, From, To, G](implicitly[G[From]])
  }

  sealed trait RecoveredBounds[A, -L, +U >: L] {
    type Alias >: L <: U
    def aliasWitness: A =:= Alias
  }

  object RecoveredBounds {
    private val singleton: RecoveredBounds[Any, Nothing, Any] = new RecoveredBounds[Any, Nothing, Any] {
      override type Alias = Any
      override def aliasWitness: Any =:= Any = implicitly[Any =:= Any]
    }
    def apply[L, U >: L, A](implicit lower: L <:< A, upper: A <:< U): RecoveredBounds[A, L, U] =
      singleton.asInstanceOf[RecoveredBounds[A, L, U]]
  }

  def recoverBounds[L, U >: L, A](implicit lower: L <:< A, upper: A <:< U): RecoveredBounds[A, L, U] =
    RecoveredBounds[L, U, A]

  def recoverLowerBound[L, A](implicit witness: L <:< A): RecoveredBounds[A, L, Any] =
    RecoveredBounds[L, Any, A]

  def recoverUpperBound[U, A](implicit witness: A <:< U): RecoveredBounds[A, Nothing, U] =
    RecoveredBounds[Nothing, U, A]
}
