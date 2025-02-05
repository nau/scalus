#ifndef SCALUS_H
#define SCALUS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  int64_t cpu;
  int64_t memory;
} ex_budget;

typedef void machine_params;
typedef void data;

machine_params* scalus_get_default_machine_params(int plutus_version, int protocol_version);

machine_params* scalus_get_machine_params_from_cardano_cli_protocol_params_json(const char* json, int plutus_version);

data* scalus_data_from_cbor(const char* cbor, size_t len);
data* scalus_data_from_json(const char* json);

void scalus_free(void* ptr);

int scalus_script_apply_data_arg(const char* script_hex, char* result, size_t len, data* arg);

// Evaluates a Plutus script
// Parameters:
//   script_hex: null-terminated hex string of the script
//   plutusVersion: 1 for V1, 2 for V2, 3 for V3
int scalus_evaluate_script(
    const char* script_hex,
    int plutus_version,
    machine_params* params,
    char* logs,
    size_t logs_len,
    char* error,
    size_t error_len,
    ex_budget* budget);

int scalus_flat_script_from_hex(const char* script_hex, char** script, size_t len);

extern int ScalaNativeInit(void);

#ifdef __cplusplus
}
#endif

#endif