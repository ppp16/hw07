name := "hw07"

version := "1.0"

scalaVersion := "2.11.4"

unmanagedBase := file(sys.env("HOME")) / "etc" / "figaro"

val chart = "com.quantifind" %% "wisp" % "0.0.4"

libraryDependencies += chart