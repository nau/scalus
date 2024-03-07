package scalus;

import scalus.builtin.*;
import scalus.uplc.*;
import scalus.uplc.eval.Cek;
import scalus.uplc.eval.CekMachine;
import java.nio.file.*;

class CekMachineExample {
    public static void main(String[] args) throws Exception {
        // Create a new PlutusV2 CEK machine
        var cek = new CekMachine(Cek.plutusV2Params(new JVMPlatformSpecific()));
        // Read Plutus script from a file
        var bytes = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        var prg = ProgramFlatCodec.decodeFlat(bytes);
        // Evaluate the debruijned program
        var result = cek.evalCek(prg.term());
        // Convert the result back to named terms
        result = DeBruijn.fromDeBruijnTerm(result);
        System.out.println(result);
    }
}
