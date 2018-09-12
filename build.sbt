name := "Tesi"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.3.0"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"
libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "42.2.3",
  "com.typesafe.slick" %% "slick" % "3.2.1",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1"
)
libraryDependencies += "com.typesafe.slick" %% "slick-codegen" % "3.2.1"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.5"
libraryDependencies += "com.github.wookietreiber" %% "scala-cli-tools" % "0.2.0"
// https://mvnrepository.com/artifact/org.slf4j/slf4j-log4j12
//libraryDependencies += "log4j" % "log4j" % "1.2.17"
libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.25"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"
scalacOptions += "-Ypartial-unification"

mainClass in assembly := Some("Enricher.main")