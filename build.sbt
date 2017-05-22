name := """scala_sandbox"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.chuusai"   %% "shapeless"  % "2.3.2",
  "org.typelevel" %% "cats"       % "0.9.0",
  "org.scalatest" %% "scalatest"  % "2.2.4"     % "test"
)

