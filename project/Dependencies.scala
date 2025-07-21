import sbt.*

object Dependencies {

  object V {
    val tessellation: String = sys.env.getOrElse("TESSELLATION_VERSION", "3.4.0-rc.0")
    val decline = "2.4.1"
    val organizeImports = "0.5.0"
  }

  def tessellation(artifact: String): ModuleID = {
    val version = V.tessellation
    println(s"Using tessellation version: $version") // Debug logging
    "io.constellationnetwork" %% s"tessellation-$artifact" % version
  }

  def decline(artifact: String = ""): ModuleID =
    "com.monovore" %% {
      if (artifact.isEmpty) "decline" else s"decline-$artifact"
    } % V.decline

  object Libraries {
    val tessellationSdk = tessellation("sdk")
    val declineCore = decline()
    val declineEffect = decline("effect")
    val declineRefined = decline("refined")
    val requests = "com.lihaoyi" %% "requests" % "0.8.0"
    val upickle = "com.lihaoyi" %% "upickle" % "3.1.3"
    val catsEffectTestkit = "org.typelevel" %% "cats-effect-testkit" % "3.4.7"
    val weaverCats = "com.disneystreaming" %% "weaver-cats" % "0.8.3"
    val weaverDiscipline = "com.disneystreaming" %% "weaver-discipline" % "0.8.3"
    val weaverScalaCheck = "com.disneystreaming" %% "weaver-scalacheck" % "0.8.3"
    val organizeImports = "com.github.liancheng" %% "organize-imports" % V.organizeImports
  }

  object CompilerPlugin {

    val betterMonadicFor = compilerPlugin(
      "com.olegpy" %% "better-monadic-for" % "0.3.1"
    )

    val kindProjector = compilerPlugin(
      ("org.typelevel" % "kind-projector" % "0.13.3").cross(CrossVersion.full)
    )

    val semanticDB = compilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.13.1.1").cross(CrossVersion.full)
    )
  }
}
