name := "srtm-hgt-reader"
organization := "com.soulfiresoft"
version := "1.0.0"
scalaVersion := "2.13.10"

crossScalaVersions := Seq("2.13.10", "2.12.17", "2.11.12")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
)

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)
