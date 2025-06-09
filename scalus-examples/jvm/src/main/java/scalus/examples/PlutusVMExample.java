package scalus.examples;

import scala.math.BigInt;
import scalus.builtin.Data;
import scalus.builtin.JVMPlatformSpecific$;
import scalus.ledger.api.PlutusLedgerLanguage;
import scalus.ledger.api.ProtocolVersion;
import scalus.uplc.DeBruijnedProgram;
import scalus.uplc.eval.*;

import java.nio.file.Paths;
import java.util.Arrays;

import static java.nio.file.Files.readAllBytes;
import static java.nio.file.Files.readString;

class PlutusVMExample {

    public static void evaluationExample1() throws Exception {
        // Read script from a double-CBOR-encoded hex string
        var script = DeBruijnedProgram.fromDoubleCborHex("545301010023357389210753756363657373004981");
        var appliedScript = script.applyArg(Data.I.apply(BigInt.apply(42)));
        // Evaluate the script
        var machineParams = MachineParams.defaultParamsFor(PlutusLedgerLanguage.PlutusV3, ProtocolVersion.conwayPV());
        var plutusVM = PlutusVM.makePlutusV3VM(machineParams, JVMPlatformSpecific$.MODULE$);
        var result = plutusVM.evaluateScriptDebug(appliedScript);
        System.out.println("Evaluation Result:" + result);
        System.out.println("Is success: " + result.isSuccess());
        System.out.println("Budget used: " + result.budget());
    }

    public static void evaluationExample2() throws Exception {
        // Read Plutus script from a file
        var flatScript = readAllBytes(Paths.get("../../bench/src/main/resources/data/auction_1-1.flat"));
        var script = DeBruijnedProgram.fromFlatEncoded(flatScript);
        // Evaluate the script
        var pparams = readString(Paths.get("../../scalus-core/shared/src/main/resources/protocol-params.json"));
        var machineParams = MachineParams.fromCardanoCliProtocolParamsJson(pparams, PlutusLedgerLanguage.PlutusV2);
        var plutusVM = PlutusVM.makePlutusV2VM(machineParams, JVMPlatformSpecific$.MODULE$);
        try {
            // Pass custom budget spender and logger
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

    public static void main(String[] args) throws Exception {
        evaluationExample1();
        evaluationExample2();
    }
}
