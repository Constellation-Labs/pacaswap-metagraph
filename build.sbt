import Dependencies.*
import sbt.*

ThisBuild / organization := "io.constellationnetwork"
ThisBuild / organizationName := "amm_metagraph"
ThisBuild / scalaVersion := "2.13.16"

ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / scalafixDependencies += Libraries.organizeImports

ThisBuild / assemblyMergeStrategy := {
  case "logback.xml"                                       => MergeStrategy.first
  case x if x.contains("io.netty.versions.properties")     => MergeStrategy.discard
  case PathList(xs @ _*) if xs.last == "module-info.class" => MergeStrategy.first
  case x if x.contains("rally-version.properties")         => MergeStrategy.concat
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

lazy val commonTestSettings = Seq(
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  scalafmtOnCompile := true,
  scalafixOnCompile := true,
  libraryDependencies ++= Seq(
    Libraries.weaverCats,
    Libraries.weaverDiscipline,
    Libraries.weaverScalaCheck,
    Libraries.catsEffectTestkit
  ).map(_ % Test)
)

lazy val root = (project in file("."))
  .settings(
    name := "amm_metagraph"
  )
  .aggregate(sharedData, currencyL0, currencyL1, dataL1)

lazy val sharedData = (project in file("modules/shared_data"))
  .enablePlugins(AshScriptPlugin)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "amm_metagraph-shared_data",
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.amm_metagraph.shared_data",
    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.githubPackages("abankowski", "http-request-signer"),
    Defaults.itSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      Libraries.tessellationSdk,
      Libraries.requests,
      Libraries.upickle
    )
  )
lazy val currencyL1 = (project in file("modules/l1"))
  .enablePlugins(AshScriptPlugin)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "amm_metagraph-currency-l1",
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.amm_metagraph.l1",
    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.githubPackages("abankowski", "http-request-signer"),
    Defaults.itSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      Libraries.tessellationSdk
    )
  )

lazy val currencyL0 = (project in file("modules/l0"))
  .enablePlugins(AshScriptPlugin)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging)
  .dependsOn(sharedData)
  .settings(
    name := "amm_metagraph-currency-l0",
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.amm_metagraph.l0",
    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.githubPackages("abankowski", "http-request-signer"),
    Defaults.itSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      Libraries.declineRefined,
      Libraries.declineCore,
      Libraries.declineEffect,
      Libraries.tessellationSdk
    )
  )

lazy val dataL1 = (project in file("modules/data_l1"))
  .enablePlugins(AshScriptPlugin)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging)
  .dependsOn(sharedData)
  .settings(
    name := "amm_metagraph-data_l1",
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.amm_metagraph.data_l1",
    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.githubPackages("abankowski", "http-request-signer"),
    Defaults.itSettings,
    commonTestSettings,
    libraryDependencies ++= Seq(
      CompilerPlugin.kindProjector,
      CompilerPlugin.betterMonadicFor,
      CompilerPlugin.semanticDB,
      Libraries.tessellationSdk
    )
  )
