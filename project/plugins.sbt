logLevel := Level.Warn

// Fix https://github.com/coursier/coursier/issues/450
classpathTypes += "maven-plugin"

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
