name := """scala_sandbox"""

version := "1.0"

scalaVersion := "2.11.7"

fork in run := true
cancelable in Global := true

libraryDependencies ++= Seq(
  "com.chuusai"    %% "shapeless"             % "2.3.2",
  "org.typelevel"  %% "cats"                  % "0.9.0",
  "com.twitter"    % "util-core_2.11"         % "17.11.0",
  "org.scalacheck" %% "scalacheck"            % "1.12.1" % "test",
  "org.scalatest"  %% "scalatest"             % "3.0.0"  % "test"
)

