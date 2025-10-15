package scalus.cardano.ledger

case class SlotConfig(zeroTime: Long, zeroSlot: Long, slotLength: Long) {
    def slotToTime(slot: Long): Long = zeroTime + (slot - zeroSlot) * slotLength
    def timeToSlot(time: Long): Long = zeroSlot + ((time - zeroTime) / slotLength)
}

object SlotConfig {
    // taken from https://github.com/spacebudz/lucid/blob/main/src/plutus/time.ts
    val Mainnet: SlotConfig = SlotConfig(
      zeroTime = 1596059091000L,
      zeroSlot = 4492800,
      slotLength = 1000
    ) // Starting at Shelley era
    val Preview: SlotConfig = SlotConfig(
      zeroTime = 1666656000000L,
      zeroSlot = 0,
      slotLength = 1000
    ) // Starting at Shelley era
    val Preprod: SlotConfig = SlotConfig(
      zeroTime = 1654041600000L + 1728000000L,
      zeroSlot = 86400,
      slotLength = 1000
    )
}
