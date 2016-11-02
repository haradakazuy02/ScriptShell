organization := "jp.gr.java_conf.harada"

name := "scriptshell"

version := "0.2-SNAPSHOT"

val scalaVer = "2.12.0"

scalaVersion := scalaVer

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVer,
  "org.scala-lang" % "scala-compiler" % scalaVer,
  "org.scala-lang" % "scala-reflect" % scalaVer)

enablePlugins(JavaAppPackaging)
