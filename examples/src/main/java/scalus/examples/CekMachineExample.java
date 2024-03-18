package scalus.examples;

import java.nio.file.Files;
import java.nio.file.Paths;

import scalus.builtin.*;
import scalus.uplc.*;
import scalus.uplc.eval.Cek;
import scalus.uplc.eval.CekMachine;
import scalus.uplc.eval.VM;

class CekMachineExample {
    public static void main(String[] args) throws Exception {
        // Read Plutus script from a file
        var bytes = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        // Evaluate the script
        var result = VM.evaluateScriptCounting(VM.plutusV2Params(), bytes);
        System.out.println(result);
    }
}
