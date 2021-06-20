import sbt._
import Keys._
import xerial.sbt.Sonatype._

inThisBuild(
  Seq(
    organization := "com.github.mvv.typine",
    version := "0.1-SNAPSHOT", // next is M3
    homepage := Some(url("https://github.com/mvv/typine")),
    scmInfo := Some(ScmInfo(url("https://github.com/mvv/typine"), "scm:git@github.com:mvv/typine.git")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(id = "mvv",
                name = "Mikhail Vorozhtsov",
                email = "mikhail.vorozhtsov@gmail.com",
                url = url("https://github.com/mvv"))
    ),
    sonatypeProjectHosting := Some(GitHubHosting("mvv", "typine", "mikhail.vorozhtsov@gmail.com"))
  )
)

ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / publishMavenStyle := true

lazy val sonatypeBundleReleaseIfNotSnapshot: Command = Command.command("sonatypeBundleReleaseIfNotSnapshot") { state =>
  val extracted = Project.extract(state)
  if (extracted.get(isSnapshot)) {
    val log = extracted.get(sLog)
    log.info("Snapshot version, doing nothing")
    state
  } else {
    Command.process("sonatypeBundleRelease", state)
  }
}

inThisBuild(
  Seq(
    crossScalaVersions := Seq("3.0.0", "2.13.6", "2.12.14"),
    scalaVersion := crossScalaVersions.value.head,
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xfatal-warnings")
  )
)

def scala2Minor(version: String): Option[Long] =
  CrossVersion.partialVersion(version) match {
    case Some((2, m)) => Some(m)
    case _            => None
  }

def isScala2_12(version: String): Boolean =
  scala2Minor(version).contains(12)

def isScala2_13(version: String): Boolean =
  scala2Minor(version).contains(13)

def isScala2(version: String): Boolean =
  scala2Minor(version).isDefined

lazy val typine = (project in file("."))
  .settings(
    name := "typine",
    description := "Type inequality witnesses for Scala",
    sonatypeProfileName := "com.github.mvv",
    sonatypeSessionName := s"Typine_${version.value}",
    commands += sonatypeBundleReleaseIfNotSnapshot,
    Compile / scalaSource := {
      if (isScala2(scalaVersion.value)) {
        baseDirectory.value / "src" / "main" / "scala2"
      } else {
        baseDirectory.value / "src" / "main" / "scala3"
      }
    },
    Compile / unmanagedSourceDirectories ++= {
      scala2Minor(scalaVersion.value) match {
        case Some(m) => Seq(baseDirectory.value / "src" / "main" / s"scala2_$m")
        case None    => Nil
      }
    },
    Test / scalaSource := {
      if (isScala2(scalaVersion.value)) {
        baseDirectory.value / "src" / "test" / "scala2"
      } else {
        baseDirectory.value / "src" / "test" / "scala3"
      }
    },
    scalacOptions ++= {
      if (isScala2_13(scalaVersion.value)) {
        Seq("-Ymacro-annotations")
      } else {
        Nil
      }
    },
    libraryDependencies ++= {
      if (isScala2(scalaVersion.value)) {
        Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
      } else {
        Nil
      }
    },
    libraryDependencies ++= {
      if (isScala2_12(scalaVersion.value)) {
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
      } else {
        Nil
      }
    },
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )
