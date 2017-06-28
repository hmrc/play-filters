/*
 * Copyright 2015 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt.Keys._
import sbt._
import uk.gov.hmrc.versioning.SbtGitVersioning

object HmrcBuild extends Build {

  import uk.gov.hmrc._

  val appName = "play-filters"

  lazy val microservice = Project(appName, file("."))
    .enablePlugins(SbtAutoBuildPlugin, SbtGitVersioning)
    .settings(
      scalaVersion := "2.11.7",
      libraryDependencies ++= AppDependencies(),
      crossScalaVersions := Seq("2.11.7"),
      resolvers := Seq(
        Resolver.bintrayRepo("hmrc", "releases"),
        Resolver.typesafeRepo("releases")
      )
    )
    .disablePlugins(sbt.plugins.JUnitXmlReportPlugin)
}

private object AppDependencies {

  import play.core.PlayVersion

  val compile = Seq(
    "com.typesafe.play" %% "play" % PlayVersion.current % "provided",
    "uk.gov.hmrc" %% "http-verbs-core" % "0.1-SNAPSHOT"

  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply() = new TestDependencies {
      override lazy val test = Seq(
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
        "com.typesafe.play" %% "play-specs2" % PlayVersion.current % scope,
        "org.scalatest" %% "scalatest" % "2.2.4" % scope,
        "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" %  scope,
        "org.scalactic" %% "scalactic" % "2.2.2" % scope,
        "org.pegdown" % "pegdown" % "1.5.0" % scope,
        "org.mockito" % "mockito-all" % "1.9.5" % "test"
      )
    }.test
  }

  def apply() = compile ++ Test()
}
