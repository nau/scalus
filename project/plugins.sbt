resolvers ++= Resolver.sonatypeOssRepos("snapshots")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.15.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.5.2")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
// show welcome message
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.3.2")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
