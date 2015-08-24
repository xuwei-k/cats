import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import pl.project13.scala.sbt.SbtJmh._
import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._
import ScoverageSbtPlugin._

import org.scalajs.sbtplugin.cross.CrossProject

lazy val scoverageSettings = Seq(
  ScoverageKeys.coverageMinimum := 60,
  ScoverageKeys.coverageFailOnMinimum := false,
  ScoverageKeys.coverageHighlighting := scalaBinaryVersion.value != "2.10",
  ScoverageKeys.coverageExcludedPackages := "cats\\.bench\\..*"
)

lazy val buildSettings = Seq(
  organization := "org.spire-math",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= Seq(
    "bintray/non" at "http://dl.bintray.com/non/maven",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %%% "simulacrum" % "0.4.0",
    "org.spire-math" %%% "algebra" % "0.3.1",
    "org.spire-math" %%% "algebra-std" % "0.3.1",
    "org.typelevel" %%% "machinist" % "0.4.1",
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.0-M5" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/non/cats"),
    "scm:git:git@github.com:non/cats.git"))
) ++ warnUnusedImport

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val catsSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val disciplineDependencies = Seq(
  libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.12.4",
  libraryDependencies += "org.typelevel" %%% "discipline" % "0.4"
)

lazy val docSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(coreJVM, freeJVM, stateJVM),
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api"),
  site.addMappingsToSiteDir(tut, "_tut"),
  ghpagesNoJekyll := false,
  siteMappings += file("CONTRIBUTING.md") -> "contributing.md",
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo := "git@github.com:non/cats.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md"
)

lazy val docs = project
  .settings(moduleName := "cats-docs")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(unidocSettings)
  .settings(site.settings)
  .settings(ghpages.settings)
  .settings(docSettings)
  .settings(tutSettings)
  .settings(tutScalacOptions ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code"))))
  .settings(commonJvmSettings)
  .dependsOn(coreJVM, freeJVM, stateJVM)

lazy val cats = project.in(file("."))
  .settings(moduleName := "root")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .aggregate(catsJVM, catsJS)
  .dependsOn(catsJVM, catsJS, testsJVM % "test-internal -> test", bench % "compile-internal;test-internal -> test")

lazy val catsJVM = project.in(file(".catsJVM"))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .aggregate(macrosJVM, coreJVM, lawsJVM, testsJVM, docs, freeJVM, bench, stateJVM)
  .dependsOn(macrosJVM, coreJVM, lawsJVM, testsJVM, docs, freeJVM, bench, stateJVM)

lazy val catsJS = project.in(file(".catsJS"))
  .settings(moduleName := "cats")
  .settings(catsSettings)
  .aggregate(macrosJS, coreJS, lawsJS, testsJS, freeJS, stateJS)
  .dependsOn(macrosJS, coreJS, lawsJS, testsJS, freeJS, stateJS)
  .enablePlugins(ScalaJSPlugin)

lazy val macros = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "cats-macros")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val macrosJVM = macros.jvm 
lazy val macrosJS = macros.js

lazy val core = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros)
  .settings(moduleName := "cats-core")
  .settings(catsSettings:_*)
  .settings(
    sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
  )
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject
  .dependsOn(macros, core)
  .settings(moduleName := "cats-laws")
  .settings(catsSettings:_*)
  .settings(disciplineDependencies:_*)
  .settings(libraryDependencies += "org.spire-math" %%% "algebra-laws" % "0.3.1")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val bench = project.dependsOn(macrosJVM, coreJVM, freeJVM, lawsJVM)
  .settings(moduleName := "cats-bench")
  .settings(catsSettings)
  .settings(noPublishSettings)
  .settings(jmhSettings)
  .settings(commonJvmSettings)

lazy val free = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, tests % "test-internal -> test")
  .settings(moduleName := "cats-free")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val freeJVM = free.jvm
lazy val freeJS = free.js

lazy val state = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, free, tests % "test-internal -> test")
  .settings(moduleName := "cats-state")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val stateJVM = state.jvm
lazy val stateJS = state.js

lazy val task = crossProject.crossType(CrossType.Pure)
  .dependsOn(macros, core, tests % "test-internal -> test")
  .settings(moduleName := "cats-task")
  .settings(catsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val taskJVM = task.jvm
lazy val taskJS = task.js

lazy val tests = crossProject
  .dependsOn(macros, core, laws)
  .settings(moduleName := "cats-tests")
  .settings(catsSettings:_*)
  .settings(disciplineDependencies:_*)
  .settings(noPublishSettings:_*)
  .settings(libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M7" % "test")
  .settings(libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5" % "test")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .settings(scalapropsWithScalazlaws:_*)
  .settings(scalapropsVersion := "0.1.13")

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/non/cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  autoAPIMappings := true,
  apiURL := Some(url("https://non.github.io/cats/api/")),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>non</id>
        <name>Erik Osheim</name>
        <url>http://github.com/non/</url>
      </developer>
    </developers>
  )
)

// These aliases serialise the build for the benefit of Travis-CI.
addCommandAlias("buildJVM", ";macrosJVM/compile;coreJVM/compile;freeJVM/compile;freeJVM/test;stateJVM/compile;stateJVM/test;lawsJVM/compile;testsJVM/test;docs/test;bench/test")

addCommandAlias("validateJVM", ";buildJVM;scalastyle;buildJVM;scalastyle;unidoc;tut")

addCommandAlias("validateJS", ";macrosJS/compile;coreJS/compile;lawsJS/compile;testsJS/test;freeJS/compile;freeJS/test;stateJS/compile;stateJS/test")

addCommandAlias("validate", ";validateJS;validateJVM")

////////////////////////////////////////////////////////////////////////////////////////////////////
// Base Build Settings - Should not need to edit below this line. 
// These settings could also come from another file or a plugin.
// The only issue if coming from a plugin is that the Macro lib versions
// are hard coded, so an overided facility would be required.

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" %%% "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" %% "paradise" % "2.0.1" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary
        )
    }
  }
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val sharedPublishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  }
)
 
lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges)
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
)

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
