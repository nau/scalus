#include "scalus.h"
#include <assert.h>
#include <stdio.h>

// Example Plutus script in hex format
// (d: Data) => log("Success")
#define TEST_SCRIPT "545301010023357389210753756363657373004981"
// trace("Failure")(false)
#define TEST_FAILING_SCRIPT "54530101003357389201074661696c757265004a01"
#define UNIT_DATA "{\"constructor\":0,\"fields\":[]}"

void run_script(const char* script)
{
    // get default machine params for Plutus V3 and Cardano protocol version 10
    machine_params* params = scalus_get_default_machine_params(3, 10);
    assert(params != NULL);

    // read a cardano-cli protocol-params.json file into a string
    FILE* fp = fopen("../../scalus-core/shared/src/main/resources/protocol-params.json", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        return;
    }
    char json[20000];
    fread(json, 1, sizeof(json), fp);
    fclose(fp);
//    printf("JSON: %s\n", json);

    // get machine params from cardano-cli protocol-params.json file
    machine_params* params2 = scalus_get_machine_params_from_cardano_cli_protocol_params_json(json, 3);
    assert(params2 != NULL);

    // evaluate the script with the default machine params
    ex_budget budget;
    char logs_buffer[1024];
    char error[1024];
    int ret = scalus_evaluate_script(
        script, // script hex
        3, // Plutus V3
        params2, // machine params
        &budget,
        logs_buffer,
        sizeof(logs_buffer),
        error,
        sizeof(error));

    // free the machine params
    scalus_free(params);
    scalus_free(params2);

    if (ret == 0) {
        printf("Script evaluation successful. CPU %lld, MEM %lld\n", budget.cpu, budget.memory);
        printf("Logs: %s\n", logs_buffer);
    } else {
        printf("Script evaluation failed: %d\n", ret);
        printf("Units spent: CPU %lld, MEM %lld\n", budget.cpu, budget.memory);
        printf("Error: %s\n", error);
        printf("Logs: %s\n", logs_buffer);
    }
}

int main()
{
    // This function needs to be called before invoking any methods defined in Scala Native.
    // Might be called automatically unless SCALANATIVE_NO_DYLIB_CTOR env variable is set.
    assert(ScalaNativeInit() == 0);
    // Read Plutus Data from JSON string
    data* unit_data = scalus_data_from_json(UNIT_DATA);
    assert(unit_data != NULL);

    char applied_script[1024];
    // Our example script is a function that takes a Plutus Data argument and logs "Success".
    // We apply the script to the data we created above.
    // The resulting script is a hex-encoded string of double CBOR flat-encoded Plutus script.
    assert(scalus_script_apply_data_arg(TEST_SCRIPT, applied_script, sizeof(applied_script), unit_data) == 0);
    printf("Applied script: %s\n", applied_script);

    run_script(applied_script);
    run_script(TEST_FAILING_SCRIPT);
    // Free the Plutus Data object
    scalus_free(unit_data);
    return 0;
}