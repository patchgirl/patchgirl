import Dependency._

name := """back"""
organization := "com.tsu"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.8"

libraryDependencies += guice
libraryDependencies ++= apiTest
libraryDependencies ++= api
libraryDependencies ++= db
