#ifndef SCALUS_H
#define SCALUS_H

#ifdef __cplusplus
extern "C" {
#endif

// Evaluates a Plutus script
// Parameters:
//   scriptHex: null-terminated hex string of the script
//   plutusVersion: 1 for V1, 2 for V2, 3 for V3
int scalus_evaluate_script(const char* scriptHex, int plutusVersion);

//int ScalaNativeInit(void);

#ifdef __cplusplus
}
#endif

#endif