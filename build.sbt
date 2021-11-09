name := "Chip8V3"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

Compile / mainClass := Some("Chip8")
assembly / mainClass := Some("Chip8")