package scalus.builtin

// We copy several files from the shared project to the plugin project during sbt build
// as it's not that easy to add dependencies for Scala Plugins.
// This is a workaround to avoid original DataApi transitive dependencies.
// Just leave the trait empty. It's needed for Data to compile.
trait DataApi {}
