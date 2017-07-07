name := "graphParser"

organization := "edu.berkeley.nlp"

version := "0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-Yno-adapted-args",          // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-dead-code",           // Warn when dead code is identified. (internet says his doesn't work well with the ??? hole)
  "-Ywarn-infer-any",           // Warn when a type argument is inferred to be `Any`.
///  "-Yinline",
///  "-optimise",
///  "-Xdisable-assertions" // For generating a marginally faster version
///  "-Xfuture",
///  "-Xlint",
///  "-Ywarn-unused",              // Warn when local and private vals, vars, defs, and types are unused.
///  "-Ywarn-unused-import",        // Warn when imports are unused.
///  "-Ywarn-numeric-widen"       // Warn when numerics are widened.
///  "-Yinline-warnings",           // Emit inlining warnings. (Normally suppressed due to high volume)
  "-deprecation",
  "-feature",
  "-unchecked"
)


libraryDependencies  ++= Seq(
  // other dependencies here as:  groupID % artifactID % revision % configuration
)

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

// If you need scalapb/scalapb.proto or anything from
// google/protobuf/*.proto
libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion % "protobuf"

