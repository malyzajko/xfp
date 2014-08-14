name := "Xfp"

version := "0.0"

scalaVersion := "2.10.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"
