import java.io.File

import com.banno.license.Plugin.LicenseKeys._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import sbt.Keys._
import sbt._

/**
 * sbt \
 * -Dsbt.global.staging=sbt.staging \
 * -DOTI_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory
 */
object OTITrees extends Build {
  
  // ======================

  lazy val otiSettings = Seq(
    scalaVersion := Versions.scala,
    organization := "gov.nasa.jpl.mbee.omg.oti",
    organizationName := "JPL, Caltech",
    organizationHomepage := Some(url("https://mbse.jpl.nasa.gov")),
    publishMavenStyle := false,
    publishTo := {
      Option.apply(System.getProperty("OTI_LOCAL_REPOSITORY")) match {
        case Some(dir) => Some(Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns))
        case None => sys.error("Set -DOTI_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    },
    resolvers += {
      Option.apply(System.getProperty("OTI_LOCAL_REPOSITORY")) match {
        case Some(dir) => Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns)
        case None => sys.error("Set -DOTI_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    }
  )

  lazy val commonSettings =
    Defaults.coreDefaultSettings ++
      Defaults.runnerSettings ++
      Defaults.baseTasks ++
      graphSettings ++
      com.banno.license.Plugin.licenseSettings ++
      Seq(
        sourceDirectories in Compile ~= { _.filter(_.exists) },
        sourceDirectories in Test ~= { _.filter(_.exists) },
        unmanagedSourceDirectories in Compile ~= { _.filter(_.exists) },
        unmanagedSourceDirectories in Test ~= { _.filter(_.exists) },
        unmanagedResourceDirectories in Compile ~= { _.filter(_.exists) },
        unmanagedResourceDirectories in Test ~= { _.filter(_.exists) }
      )
      
  lazy val oti_trees = Project(
    "oti-trees",
    file(".")).
    settings(otiSettings: _*).
    settings(commonSettings: _*).
    settings(
        version := Versions.version,
        removeExistingHeaderBlock := true,
        libraryDependencies ++= Seq(
          "org.scala-lang" % "scala-reflect" % Versions.scala % "provided" withSources() withJavadoc(),
          "org.scala-lang" % "scala-library" % Versions.scala % "provided" withSources() withJavadoc(),
          "org.scala-lang" % "scala-compiler" % Versions.scala % "provided" withSources() withJavadoc(),
          "gov.nasa.jpl.mbee.omg.oti" %% "oti-core" % Versions.oti_core_version withSources() withJavadoc()
        ),
        scalaSource in Compile := baseDirectory.value / "src",
        classDirectory in Compile := baseDirectory.value / "bin",
        shellPrompt := { state => Project.extract(state).currentRef.project + " @ " + Versions.version_suffix + "> " }
    )
    
}
