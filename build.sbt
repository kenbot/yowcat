
scalaVersion := "2.11.6"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

initialCommands := """
  import kenbot.yowcat._;
  import Sets._
"""

scalacOptions ++= Seq( 
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:existentials",
  "-language:higherKinds")
