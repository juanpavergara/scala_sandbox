name := """scala_sandbox"""

version := "1.0"

scalaVersion := "2.11.7"

val catsVersion = "0.9.0"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats"       % catsVersion         ,
  "org.scalatest" %% "scalatest"  % "2.2.4"     % "test"
)

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

