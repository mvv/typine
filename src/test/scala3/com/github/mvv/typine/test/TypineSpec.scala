package com.github.mvv.typine.test

import com.github.mvv.typine.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.*

class TypineSpec extends AnyFlatSpec:
  "!:=" must "see that simple types differ" in {
    "summon[String !:= Char]" must compile
  }

  // FIXME: Doesn't actually compile, but the matcher fails
  /*
  it must "fail to differentiate type parameters" in {
    "def f[A, B] = summon[A !:= B]" mustNot compile
  }
  */

  it must "differentiate simple types from applied ones" in {
    "def f[A] = summon[Option[A] !:= Int]" must compile
  }