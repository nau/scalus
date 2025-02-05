#include <assert.h>
#include <stdio.h>
#include "scalus.h"

// Example Plutus script in hex format
#define TEST_SCRIPT "545301010033573892010753756363657373004981"
#define TEST_FAILING_SCRIPT "54530101003357389201074661696c757265004a01"

void run_script(const char* script) {
    ex_budget budget;

    char buffer[1024];
    char* ptr = buffer;
    int r = scalus_flat_script_from_hex(script, &ptr, sizeof(buffer));
    printf("Flat script result %d: %s\n", r, buffer);

    machine_params* params = scalus_get_default_machine_params(3, 10);
    assert(params != NULL);

    char error[1024];
    int ret = scalus_evaluate_script(
        script,    // script hex
        3,         // Plutus V3
        params,    // machine params
        buffer,
        sizeof(buffer),
        error,
        sizeof(error),
        &budget    // result struct
    );

    scalus_free_machine_params(params);

    if (ret == 0) {
        printf("Script evaluation successful. CPU %lld, MEM %lld\n", budget.cpu, budget.memory);
        printf("Logs: %s\n", buffer);
    } else {
        printf("Script evaluation failed: %d\n", ret);
        printf("Units spent: CPU %lld, MEM %lld\n", budget.cpu, budget.memory);
        printf("Error: %s\n", error);
        printf("Logs: %s\n", buffer);
    }
}

int main() {
    // This function needs to be called before invoking any methods defined in Scala Native.
    // Might be called automatically unless SCALANATIVE_NO_DYLIB_CTOR env variable is set.
    assert(ScalaNativeInit() == 0);
    run_script(TEST_SCRIPT);
    run_script(TEST_FAILING_SCRIPT);
    return 0;
}