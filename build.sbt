import xsbti.compile.CompileOrder.JavaThenScala

name := "BTreeMap"

version := "0.1"

scalaVersion := "2.12.8"

enablePlugins(JmhPlugin)

libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

// coverageEnabled in Test := true

compileOrder := JavaThenScala
