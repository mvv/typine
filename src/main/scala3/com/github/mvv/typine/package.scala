package com.github.mvv.typine

import com.github.mvv.typine.impl.TypineMacros

import scala.annotation.implicitNotFound

@implicitNotFound("could not prove that types ${A} and ${B} are different")
sealed trait !:=[A, B]

object `!:=`:
  private val singleton = new !:=[Any, Any]:
    override def toString: String = "!:="
  def unsafeMake[A, B]: A !:= B = singleton.asInstanceOf[A !:= B]
  inline given derive[A, B]: (A !:= B) = ${TypineMacros.deriveUnequal[A, B]}

def substituteCoBounded[L, U >: L, From >: L <: U, To >: L <: U, F[+_ >: L <: U]](f: F[From])(using From <:< To): F[To] =
  f.asInstanceOf[F[To]]

def substituteContraBounded[L, U >: L, From >: L <: U, To >: L <: U, F[-_ >: L <: U]](f: F[To])(using From <:< To): F[From] =
  f.asInstanceOf[F[From]]

def substituteBounded[L, U >: L, From >: L <: U, To >: L <: U, F[_ >: L <: U]](f: F[From])(using From =:= To): F[To] =
  f.asInstanceOf[F[To]]

def liftCoBounded[L, U >: L, From >: L <: U, To >: L <: U, F[+_ >: L <: U]](using From <:< To): F[From] <:< F[To] =
  type G[+T >: L <: U] = F[From] <:< F[T]
  substituteCoBounded[L, U, From, To, G](summon[G[From]])

def liftContraBounded[L, U >: L, From >: L <: U, To >: L <: U, F[-_ >: L <: U]](using From <:< To): F[To] <:< F[From] =
  type G[-T >: L <: U] = F[To] <:< F[T]
  substituteContraBounded[L, U, From, To, G](summon[G[To]])

def liftBounded[L, U >: L, From >: L <: U, To >: L <: U, F[_ >: L <: U]](using From =:= To): F[From] =:= F[To] =
  type G[T >: L <: U] = F[From] =:= F[T]
  substituteBounded[L, U, From, To, G](summon[G[From]])

sealed trait RecoveredBounds[A, -L, +U]:
  type Alias >: (L | A) <: (U & A)

object RecoveredBounds:
  private val singleton: RecoveredBounds[Any, Nothing, Any] = new RecoveredBounds[Any, Nothing, Any]:
    override type Alias = Any
  def apply[L, U, A](using L <:< A, A <:< U): RecoveredBounds[A, L, U] =
    singleton.asInstanceOf[RecoveredBounds[A, L, U]]

def recoverBounds[L, U, A](using L <:< A, A <:< U): RecoveredBounds[A, L, U] =
  RecoveredBounds[L, U, A]

def recoverLowerBound[L, A](using L <:< A): RecoveredBounds[A, L, Any] =
  RecoveredBounds[L, Any, A]

def recoverUpperBound[U, A](using A <:< U): RecoveredBounds[A, Nothing, U] =
  RecoveredBounds[Nothing, U, A]
