package com.github.mvv.typine.test

import com.github.mvv.typine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._

class TypineSpec extends AnyFlatSpec {
  "!:=" must "see that simple types differ" in {
    "implicitly[String !:= Char]" must compile
  }

  it must "fail to differentiate type parameters" in {
    "def f[A, B] = implicitly[A !:= B]" mustNot compile
  }

  it must "fail to differentiate type parameters in type arguments" in {
    "def f[A, B] = implicitly[Option[A] !:= Option[B]]" mustNot compile
  }

  it must "use symmetry" in {
    "def f[A, B](implicit w: A !:= B) = implicitly[B !:= A]" must compile
  }

  it must "use symmetry in type arguments" in {
    "def f[A, B](implicit w: A !:= B) = implicitly[Option[B] !:= Option[A]]" must compile
  }

  it must "differentiate simple types from applied ones" in {
    "def f[A] = implicitly[Option[A] !:= Int]" must compile
  }
}
