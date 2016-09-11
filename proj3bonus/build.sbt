name := "Pi"
 
version := "1.0"
fork in run := true 
scalaVersion := "2.11.7"
 
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "RoundEights" at "http://maven.spikemark.net/roundeights"
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

scalacOptions +="-deprecation"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" %"2.4-SNAPSHOT"
libraryDependencies += "com.roundeights" %% "hasher" % "1.2.0"
