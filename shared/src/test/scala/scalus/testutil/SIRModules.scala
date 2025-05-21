package scalus.testutil

import scalus.sir.SIR
import scalus.sir.Module
import scalus.flat
import scalus.flat.DecoderState
import scalus.flat.FlatInstantces.given

object SIRModules {

    def load(name: String): Module = {
        val filename = name.replace('.', '/') + ".sir"
        val inputStream = getClass.getClassLoader.getResourceAsStream(filename)
        if inputStream == null then {
            throw new RuntimeException(s"Module $name not found")
        }
        val bytes = inputStream.readAllBytes()
        val decoderState = DecoderState(bytes)
        val module = flat.decode[Module](decoderState)
        module
    }

}
