import sbt._
import sbt.Keys._

import scala.Some

object Build extends Build {

	import Dependencies._

	val scalaVer = "2.11.7"

	lazy val basicSettings = Seq(
		organization 				:= "co.blocke",
		startYear 					:= Some(2015),
		scalaVersion 				:= "2.11.7",
		version 					:= "0.1",
		parallelExecution in Test 	:= false,
		scalacOptions				:= Seq("-feature", "-deprecation", "-Xlint", "-encoding", "UTF8", "-unchecked", "-Xfatal-warnings"),
		testOptions in Test += Tests.Argument("-oDF")
	)

	// configure prompt to show current project
	override lazy val settings = super.settings :+ {
		shellPrompt := { s => Project.extract(s).currentProject.id + " > " }
	}

	lazy val root = Project("jarvis", file("."))
		.settings(basicSettings: _*)
		.settings(libraryDependencies ++=
			compile(scalajack,joda) ++
			test(scalatest)
		)
}

object Dependencies {
	def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
	def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test") 

	val scalatest 		= "org.scalatest" 			%% "scalatest"		% "2.2.4"
	val scalajack		= "co.blocke"				%% "scalajack"		% "4.4.6"
	val joda 			= "joda-time"				% "joda-time"		% "2.3"
}
