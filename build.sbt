organization := "jp.gr.java_conf.harada"

name := "scriptshell"

version := "0.3-SNAPSHOT"

val scalaVer = "2.13.0"

scalaVersion := scalaVer

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVer,
  "org.scala-lang" % "scala-compiler" % scalaVer,
  "org.scala-lang" % "scala-reflect" % scalaVer)

licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

enablePlugins(JavaAppPackaging)
