organization := "jp.gr.java_conf.harada"

name := "scriptshell"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.8",
  "org.scala-lang" % "scala-compiler" % "2.11.8",
  "org.scala-lang" % "scala-reflect" % "2.11.8")

enablePlugins(JavaAppPackaging)
