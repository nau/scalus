package scalus.examples;

import scalus.builtin.Data;
import scalus.builtin.JVMPlatformSpecific$;
import scalus.ledger.api.PlutusLedgerLanguage;
import scalus.uplc.DeBruijnedProgram;
import scalus.uplc.Constant;
import scalus.uplc.Term;
import scalus.uplc.eval.*;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

class PlutusVMExample {
    public static void evaluationExample1() throws Exception {
        // Read Plutus script from a file
        var flatScript = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        var script = DeBruijnedProgram.fromFlatEncoded(flatScript);
        // Evaluate the script
        var machineParams = MachineParams.defaultPlutusV2PostConwayParams();
        var plutusVM = PlutusVM.makePlutusV2VM(machineParams, JVMPlatformSpecific$.MODULE$);
        try {
            var result = plutusVM.evaluateScript(script, NoBudgetSpender$.MODULE$, NoLogger$.MODULE$);
            System.out.println(result);
        } catch (StackTraceMachineError e) {
            String stacktrace = Arrays.stream(e.getCekStack()).map(Object::toString).reduce("", (a, b) -> a + "\n" + b);
            e.env().foreach((t) -> {
                System.out.println(t._1() + " -> " + t._2());
                return null;
            });
            System.out.println(stacktrace);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void evaluationExample2() throws Exception {
        // Read protocol parameters from Cardano CLI protocol-parameters query, like:
        // cardano-cli query protocol-parameters –mainnet –out-file protocol-params.json
        var pparams = Files.readString(Paths.get("jvm/src/main/resources/protocol-params.json"));
        var machineParams = MachineParams.fromCardanoCliProtocolParamsJson(pparams, PlutusLedgerLanguage.PlutusV2);
        // Read Plutus script from a file
        var flatScript = Files.readAllBytes(Paths.get("bench/src/main/resources/data/auction_1-1.flat"));
        var script = DeBruijnedProgram.fromFlatEncoded(flatScript);
        var arg = Term.Const.apply(Constant.Data.apply(Data.I.apply(scala.math.BigInt.apply(123))));
        var appliedScript = script.applyArg(arg);

        // Evaluate the script
        var plutusVM = PlutusVM.makePlutusV2VM(machineParams, JVMPlatformSpecific$.MODULE$);
        var result = plutusVM.evaluateScriptDebug(appliedScript);
        System.out.println(result);
    }

    public static void main(String[] args) throws Exception {
        evaluationExample1();
        evaluationExample2();
    }
}
