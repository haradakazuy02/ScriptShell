lazy val root = (project in file(".")).settings(
 organization := "jp.gr.java_conf.harada",
 name := "scriptshell",
 version := "0.2-SNAPSHOT",
 scalaVersion := "2.11.8",
 scalacOptions ++= Seq("-encoding", "MS932"),
 libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.8",
  "org.scala-lang" % "scala-compiler" % "2.11.8",
  "org.scala-lang" % "scala-reflect" % "2.11.8"),
 packAutoSettings
)
