#ifndef SCALUS_H
#define SCALUS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  long long cpu;
  long long memory;
  char* logs;
  char* error;
} eval_result;

// Evaluates a Plutus script
// Parameters:
//   script_hex: null-terminated hex string of the script
//   plutusVersion: 1 for V1, 2 for V2, 3 for V3
int scalus_evaluate_script(const char* script_hex, int plutus_version, eval_result* result);

int scalus_flat_script_from_hex(const char* script_hex, char** script, size_t len);

extern int ScalaNativeInit(void);

#ifdef __cplusplus
}
#endif

#endif