# Typine
[![Release Version](https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.mvv.typine/typine_2.13.svg)](https://oss.sonatype.org/content/repositories/releases/com/github/mvv/typine)
[![Snapshot Version](https://img.shields.io/nexus/s/https/oss.sonatype.org/com.github.mvv.typine/typine_2.13.svg)](https://oss.sonatype.org/content/repositories/snapshots/com/github/mvv/typine)
[![Build Status](https://travis-ci.com/mvv/typine.svg?branch=master)](https://travis-ci.com/mvv/typine)

Type inequality witnesses for Scala. A witness `A !:= B` guarantees that
`A` and `B` are different types, taking into account possible instantiations
of type parameters inside those types. For example,

```scala
def f[A] = summon[A !:= Int]
```

will fail to compile, but both

```scala
def f[A] = summon[Either[A, String] !:= Either[Int, Char]]
def f[A, B](using A !:= B) = summon[Option[A] !:= Option[B]]
```

will be accepted.

## Using Typine in your project

Add Typine to your dependencies

```scala
libraryDependencies += "com.github.mvv.typine" %% "typine" % "0.1-M3"
```
