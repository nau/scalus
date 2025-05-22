/**
 * @file scalus.h
 * @author Alexander Nemish
 * @brief C API for Scalus - Cardano Plutus Script Evaluation Engine
 *
 * This header provides the interface for evaluating Plutus scripts on Cardano blockchain
 * using the Scalus implementation. It supports Plutus V1, V2, and V3 versions.
 */

#ifndef SCALUS_H
#define SCALUS_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Execution budget for Plutus scripts
 */
typedef struct {
    int64_t cpu; /**< CPU units limit */
    int64_t memory; /**< Memory units limit */
} ex_budget;

/**
 * @brief Opaque type for machine parameters configuration
 */
typedef void machine_params;

/**
 * @brief Opaque type for Plutus data
 */
typedef void data;

/**
 * @brief Get default machine parameters for given Plutus and protocol versions
 *
 * @param plutus_version Plutus language version (1=V1, 2=V2, 3=V3)
 * @param protocol_version Cardano protocol version
 * @return Machine parameters pointer (must be freed with scalus_free)
 */
machine_params* scalus_get_default_machine_params(int plutus_version, int protocol_version);

/**
 * @brief Parse machine parameters from Cardano CLI protocol parameters JSON
 *
 * @param json Protocol parameters JSON string
 * @param plutus_version Plutus language version (1=V1, 2=V2, 3=V3)
 * @return Machine parameters pointer (must be freed with scalus_free)
 */
machine_params* scalus_get_machine_params_from_cardano_cli_protocol_params_json(
    const char* json,
    int plutus_version);

/**
 * @brief Create Plutus data from CBOR bytes
 *
 * @param cbor CBOR-encoded data bytes
 * @param len Length of CBOR data
 * @return Data pointer (must be freed with scalus_free)
 */
data* scalus_data_from_cbor(const char* cbor, size_t len);

/**
 * @brief Create Plutus data from JSON string
 *
 * @param json JSON string representing Plutus data
 * @return Data pointer (must be freed with scalus_free)
 */
data* scalus_data_from_json(const char* json);

/**
 * @brief Free memory allocated by Scalus functions
 *
 * @param ptr Pointer to free (machine_params, data, or other Scalus-allocated memory)
 */
void scalus_free(void* ptr);

/**
 * @brief Apply datum to a parameterized script
 *
 * @param script_hex Hex-encoded string of double CBOR flat-encoded Plutus script
 * @param result Buffer for resulting hex-encoded string of double CBOR flat-encoded Plutus script
 * @param len Result buffer length
 * @param arg Datum to apply
 * @return 0 on success, error code otherwise
 */
int scalus_script_apply_data_arg(
    const char* script_hex,
    char* result,
    size_t len,
    data* arg);

/**
 * @brief Evaluate a Plutus script
 *
 * @param script_hex Hex-encoded string of double CBOR flat-encoded Plutus script
 * @param plutus_version Plutus version (1=V1, 2=V2, 3=V3)
 * @param params Machine parameters
 * @param budget Input/output execution budget
 * @param logs Buffer for evaluation logs
 * @param logs_len Logs buffer length
 * @param error Buffer for error message
 * @param error_len Error buffer length
 * @return 0 on success, error code otherwise
 */
int scalus_evaluate_script(
    const char* script_hex,
    int plutus_version,
    machine_params* params,
    ex_budget* budget,
    char* logs,
    size_t logs_len,
    char* error,
    size_t error_len);

/**
 * @brief Initialize Scala Native runtime
 * Must be called before using any other functions
 * @return 0 on success
 */
extern int ScalaNativeInit(void);

#ifdef __cplusplus
}
#endif

#endif /* SCALUS_H */