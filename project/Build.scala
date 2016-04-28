import sbt._
import Keys._
import Tests._

object SlickBuild extends Build {

  /** Dependencies for reuse in different parts of the build */
  object Dependencies {
    val slf4j = "org.slf4j" % "slf4j-api" % "1.7.18"
    val logback = "ch.qos.logback" % "logback-classic" % "1.1.6"
    val typesafeConfig = "com.typesafe" % "config" % "1.2.1"
    val reactiveStreamsVersion = "1.0.0"
    val reactiveStreams = "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
    val reactiveStreamsTCK = "org.reactivestreams" % "reactive-streams-tck" % reactiveStreamsVersion
    val mainDependencies = Seq(slf4j, typesafeConfig, reactiveStreams)
  }

  lazy val sharedSettings = Seq(
    scalacOptions ++= List("-deprecation", "-feature", "-unchecked"),
    scalaVersion := "2.11.8"
  )

  /* Project Definitions */
  lazy val aRootProject: Project = Project(id = "root", base = file("."),
    settings = Defaults.coreDefaultSettings ++ sharedSettings ++ Seq(
      sourceDirectory := file("target/root-src"),
      publishArtifact := false,
      test := (), testOnly :=  () // suppress test status output
    )).aggregate(slickProject)

  lazy val slickProject: Project = Project(id = "slick", base = file("slick"),
    settings = Defaults.coreDefaultSettings ++ sharedSettings ++ fmppSettings ++ Seq(
      libraryDependencies ++= Dependencies.mainDependencies,
      test := (), testOnly :=  () // suppress test status output
    ))

  /* FMPP Task */
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppConfig = config("fmpp").hide
  lazy val fmppSettings = inConfig(Compile)(Seq(sourceGenerators <+= fmpp, fmpp <<= fmppTask)) ++ Seq(
    libraryDependencies ++= Seq(
      ("net.sourceforge.fmpp" % "fmpp" % "0.9.15" % fmppConfig.name).intransitive,
      "org.freemarker" % "freemarker" % "2.3.23" % fmppConfig.name,
      "oro" % "oro" % "2.0.8" % fmppConfig.name,
      "org.beanshell" % "bsh" % "2.0b5" % fmppConfig.name,
      "xml-resolver" % "xml-resolver" % "1.2" % fmppConfig.name
    ),
    ivyConfigurations += fmppConfig,
    fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank },
    mappings in (Compile, packageSrc) <++=
      (sourceManaged in Compile, managedSources in Compile, sourceDirectory in Compile) map { (base, srcs, srcDir) =>
        val fmppSrc = srcDir / "scala"
        val inFiles = fmppSrc ** "*.fm"
        (srcs pair (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles pair (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
      }
  )
  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, sourceManaged, streams, sourceDirectory) map { (cp, r, output, s, srcDir) =>
      val fmppSrc = srcDir / "scala"
      val inFiles = (fmppSrc ** "*.fm").get.toSet
      val cachedFun = FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete((output ** "*.scala").get)
        val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
          "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
        toError(r.run("fmpp.tools.CommandLine", cp.files, args, s.log))
        (output ** "*.scala").get.toSet
      }
      cachedFun(inFiles).toSeq
    }
}
