resolvers ++= Resolver.sonatypeOssRepos("snapshots")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.21.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.5.4")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
// show welcome message
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.4.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
