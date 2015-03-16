lazy val root = (project in file(".")).
  settings(
    name := "jsoup-specs2-matchers",
    version := "0.1",
    scalaVersion := "2.11.6",

    crossScalaVersions := Seq("2.10.5", "2.11.6"),

    libraryDependencies ++= Seq(
      "org.jsoup" % "jsoup" % "1.8.1",
      "org.specs2" %% "specs2-core" % "2.4.17")
  )
