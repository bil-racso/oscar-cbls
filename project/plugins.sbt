resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)


libraryDependencies ++= Seq(
  "org.jacoco" % "org.jacoco.core" % "0.5.9.201207300726" artifacts Artifact("org.jacoco.core", "jar", "jar"),
  "org.jacoco" % "org.jacoco.report" % "0.5.9.201207300726" artifacts Artifact("org.jacoco.report", "jar", "jar"))

addSbtPlugin("de.johoop" % "jacoco4sbt" % "2.1.2")

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.6.8")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")