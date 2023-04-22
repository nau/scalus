package scalus

import dotty.tools.dotc.plugins.*

class Plugin extends StandardPlugin {
  val name: String = "scalus"
  override val description: String = "Count method calls"

  def init(options: List[String]): List[PluginPhase] =
    new PhaseA :: Nil
}
