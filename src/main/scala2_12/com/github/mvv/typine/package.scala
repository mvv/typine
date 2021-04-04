package com.github.mvv

import scala.language.higherKinds

package object typine extends TypineCommon {
  private val typineEqToSubSignleton: Any <:< Any = implicitly
  implicit def typineEqToSub[From, To](implicit witness: From =:= To): From <:< To =
    typineEqToSubSignleton.asInstanceOf[From <:< To]

  implicit class TypineSubSyntax[From, To](val witness: From <:< To) extends AnyVal {
    def substituteCo[F[+_]](f: F[From]): F[To] = f.asInstanceOf[F[To]]
    def substituteContra[F[-_]](f: F[To]): F[From] = f.asInstanceOf[F[From]]
    def compose[C](f: C <:< From): C <:< To = {
      type G[+T] = C <:< T
      substituteCo[G](f)
    }
    def andThen[C](f: To <:< C): From <:< C = {
      type G[-T] = T <:< C
      substituteContra[G](f)
    }
    def liftCo[F[+_]]: F[From] <:< F[To] = {
      type G[+T] = F[From] <:< F[T]
      substituteCo[G](implicitly[G[From]])
    }
    def liftContra[F[-_]]: F[To] <:< F[From] = {
      type G[-T] = F[To] <:< F[T]
      substituteContra[G](implicitly[G[To]])
    }
  }

  implicit class TypineEqSyntax[From, To](val witness: From =:= To) extends AnyVal {
    def substituteCo[F[_]](f: F[From]): F[To] = f.asInstanceOf[F[To]]
    def substituteContra[F[_]](f: F[To]): F[From] = f.asInstanceOf[F[From]]
    def compose[C](f: C <:< From): C <:< To = {
      type G[+T] = C <:< T
      typineEqToSub(witness).substituteCo[G](f)
    }
    def compose[C](f: C =:= From): C =:= To = {
      type G[T] = C =:= T
      substituteCo[G](f)
    }
    def andThen[C](f: To <:< C): From <:< C = {
      type G[-T] = T <:< C
      typineEqToSub(witness).substituteContra[G](f)
    }
    def andThen[C](f: To =:= C): From =:= C = {
      type G[T] = T =:= C
      substituteContra[G](f)
    }
    def liftCo[F[_]]: F[From] =:= F[To] = {
      type G[T] = F[From] =:= F[T]
      substituteCo[G](implicitly[G[From]])
    }
    def liftContra[F[_]]: F[To] =:= F[From] = {
      type G[T] = F[To] =:= F[T]
      substituteContra[G](implicitly[G[To]])
    }
  }
}