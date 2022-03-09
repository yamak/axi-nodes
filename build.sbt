
ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0"

val chiselVersion = "3.5.1"

lazy val root = (project in file("."))
  .settings(
    headerLicense:= Some(HeaderLicense.MIT("2022","Yusuf YAMAK",HeaderLicenseStyle.SpdxSyntax)),
    name := "axi-nodes",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0",
      "edu.berkeley.cs" %% "chisel-iotesters" % "2.5.0"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),

  )


