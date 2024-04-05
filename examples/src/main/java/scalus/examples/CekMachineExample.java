package scalus.examples;

import scalus.builtin.Data;
import scalus.ledger.api.PlutusLedgerLanguage;
import scalus.uplc.eval.CekResult;
import scalus.uplc.eval.MachineParams;
import scalus.uplc.eval.StackTraceMachineError;
import scalus.uplc.eval.VM;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

class CekMachineExample {
    public static void evaluationExample1() throws Exception {
        // Read Plutus script from a file
        var script = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        // Evaluate the script
        var machineParams = MachineParams.defaultParams();
        try {
            var result = VM.evaluateScriptCounting(machineParams, script);
            System.out.println(result);
        } catch (StackTraceMachineError e) {
            String stacktrace = Arrays.stream(e.getCekStack()).map(Object::toString).reduce("", (a, b) -> a + "\n" + b);
            e.env().foreach((t) -> {
                System.out.println(t._1() + " -> " + t._2());
                return null;
            });
            System.out.println(stacktrace);
        }
    }

    public static void evaluationExample2() throws Exception {
        // Read protocol parameters from Cardano CLI protocol-parameters query, like:
        // cardano-cli query protocol-parameters –mainnet –out-file protocol-params.json
        var pparams = Files.readString(Paths.get("jvm/src/main/resources/protocol-params.json"));
        var machineParams = MachineParams.fromCardanoCliProtocolParamsJson(pparams, PlutusLedgerLanguage.PlutusV2);
        // Read Plutus script from a file
        var script = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        var arg = Data.I.apply(scala.math.BigInt.apply(123));
        // Evaluate the script
        var result = VM.evaluateScriptCounting(machineParams, script, arg);
        System.out.println(result);
    }

    public static void main(String[] args) throws Exception {
        evaluationExample1();
        evaluationExample2();
    }
}
