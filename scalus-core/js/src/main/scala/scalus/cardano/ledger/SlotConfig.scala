package scalus.cardano.ledger

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

@JSExportTopLevel("SlotConfig")
class SlotConfig(zeroTime: Long, zeroSlot: Long, slotLength: Long) extends js.Object {
    def slotToTime(slot: Long): Long = zeroTime + (slot - zeroSlot) * slotLength
    def timeToSlot(time: Long): Long = zeroSlot + ((time - zeroTime) / slotLength)
}

object SlotConfig {
    // taken from https://github.com/spacebudz/lucid/blob/main/src/plutus/time.ts
    @JSExportStatic
    val Mainnet: SlotConfig = SlotConfig(
      zeroTime = 1596059091000L,
      zeroSlot = 4492800,
      slotLength = 1000
    ) // Starting at Shelley era
    @JSExportStatic
    val Preview: SlotConfig = SlotConfig(
      zeroTime = 1666656000000L,
      zeroSlot = 0,
      slotLength = 1000
    ) // Starting at Shelley era
    @JSExportStatic
    val Preprod: SlotConfig = SlotConfig(
      zeroTime = 1654041600000L + 1728000000L,
      zeroSlot = 86400,
      slotLength = 1000
    )
}
