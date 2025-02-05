#include <assert.h>
#include <stdio.h>
#include "scalus.h"

// Example Plutus script in hex format
#define TEST_SCRIPT "46450101004981"
#define TEST_FAILING_SCRIPT "4746010100480081"

void run_script(const char* script) {
    eval_result result;

    char buffer[1024];
    char* ptr = buffer;
    int r = scalus_flat_script_from_hex(script, &ptr, 1024);
    printf("Flat script result %d: %s\n", r, buffer);

    machine_params params = scalus_get_default_machine_params(3, 10);

    int ret = scalus_evaluate_script(
        script,    // script hex
        3,         // Plutus V3
        &result    // result struct
    );

    if (ret == 0) {
        printf("Script evaluation successful. CPU %lld, MEM %lld\n", result.cpu, result.memory);
        printf("Logs: %s\n", result.logs);
    } else {
        printf("Script evaluation failed: %d\n", ret);
        printf("Units spent: CPU %lld, MEM %lld\n", result.cpu, result.memory);
        printf("Error: %s\n", result.error);
        printf("Logs: %s\n", result.logs);
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