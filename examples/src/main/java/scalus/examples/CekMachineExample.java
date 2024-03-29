package scalus.examples;

import scalus.builtin.Data;
import scalus.builtin.JVMPlatformSpecific$;
import scalus.ledger.api.PlutusLedgerLanguage;
import scalus.uplc.eval.MachineParams;
import scalus.uplc.eval.VM;

import java.nio.file.Files;
import java.nio.file.Paths;

class CekMachineExample {
    public static void evaluationExample() throws Exception {
        // Read protocol parameters from Cardano CLI protocol-parameters query, like:
        // cardano-cli query protocol-parameters –mainnet –out-file protocol-params.json
        var pparams = Files.readString(Paths.get("jvm/src/main/resources/protocol-params.json"));
        var machineParams = MachineParams.fromCardanoCliProtocolParamsJson(pparams,
                PlutusLedgerLanguage.PlutusV2, JVMPlatformSpecific$.MODULE$);
        // Read Plutus script from a file
        var script = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        var arg = Data.I.apply(scala.math.BigInt.apply(123));
        // Evaluate the script
        var result = VM.evaluateScriptCounting(machineParams, script, arg);
    }

    public static void main(String[] args) throws Exception {
        // Read Plutus script from a file
        var bytes = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        // Evaluate the script
        var result = VM.evaluateScriptCounting(VM.plutusV2Params(), bytes);
        System.out.println(result);
    }
}
