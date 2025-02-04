#include <assert.h>
#include <stdio.h>
#include "scalus.h"

// Example Plutus script in hex format
#define TEST_SCRIPT "4e4d01000033222220051200120011"

int main() {
    // This function needs to be called before invoking any methods defined in Scala Native.
    // Might be called automatically unless SCALANATIVE_NO_DYLIB_CTOR env variable is set.
//    assert(ScalaNativeInit() == 0);
    int result = scalus_evaluate_script(
        TEST_SCRIPT,    // script hex
        3              // Plutus V3
    );

    if (result == 0) {
        printf("Script evaluation successful\n");
//        printf("Memory units spent: %ld\n", result);
    } else {
//        printf("Script evaluation failed: %s\n", result.message);
//        printf("Memory units spent: %ld\n", result.spentBudget);
        return 1;
    }

    return 0;
}