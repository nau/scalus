You are an expert Scala engineer tasked with producing production-quality code.
I need you to write Scala 3 code that meets these requirements:

1. Production-ready: Robust error handling, proper resource management, and efficient implementations.

2. Self-explanatory: Use meaningful variable/method names and follow Scala best practices.

3. Well-commented: Add detailed comments for every non-trivial part explaining the "why" behind implementation choices.

4. Use Scala 3's indentation-based syntax (not braces) inside functions.

5. Use 4 spaces for indentation.

6. Use braces for class/trait/object and functions definitions.

7. Include appropriate unit tests demonstrating the code works as expected.

8. Handle edge cases explicitly.

9. Follow functional programming principles where appropriate (immutability, pure functions).

10. Include type annotations for public APIs.

11. Document any assumptions made during implementation.

12. Provide a brief explanation of how to use the code.

13. Use case classes and Scala 3 enums where appropriate

14. avoid using opaque types.

15. Generate one class at a time.

The code should solve the following problem:

I need you to write a Scala 3 domain classes that represent the following Cardano blockchain entities described
in the following CDDL schema:

```cddl
; This file was auto-generated from huddle. Please do not modify it directly!
; Pseudo-rule introduced by Cuddle to collect root elements
huddle_root_defs = [block
                   , transaction
                   , $kes_signature
                   , language
                   , potential_languages
                   , signkeyKES]

$hash28 = bytes .size 28

$hash32 = bytes .size 32

$kes_signature = bytes .size 448

$kes_vkey = bytes .size 32

$signature = bytes .size 64

$vkey = bytes .size 32

$vrf_cert = [bytes, bytes .size 80]

$vrf_vkey = bytes .size 32

addr_keyhash = $hash28

; address = bytes
; 
; address format:
;   [ 8 bit header | payload ];
; 
; shelley payment addresses:
;      bit 7: 0
;      bit 6: base/other
;      bit 5: pointer/enterprise [for base: stake cred is keyhash/scripthash]
;      bit 4: payment cred is keyhash/scripthash
;   bits 3-0: network id
; 
; reward addresses:
;   bits 7-5: 111
;      bit 4: credential is keyhash/scripthash
;   bits 3-0: network id
; 
; byron addresses:
;   bits 7-4: 1000
; 
;      0000: base address: keyhash28,keyhash28
;      0001: base address: scripthash28,keyhash28
;      0010: base address: keyhash28,scripthash28
;      0011: base address: scripthash28,scripthash28
;      0100: pointer address: keyhash28, 3 variable length uint
;      0101: pointer address: scripthash28, 3 variable length uint
;      0110: enterprise address: keyhash28
;      0111: enterprise address: scripthash28
;      1000: byron address
;      1110: reward account: keyhash28
;      1111: reward account: scripthash28
; 1001-1101: future formats
; 
address = h'001000000000000000000000000000000000000000000000000000000011000000000000000000000000000000000000000000000000000000'
           / h'102000000000000000000000000000000000000000000000000000000022000000000000000000000000000000000000000000000000000000'
           / h'203000000000000000000000000000000000000000000000000000000033000000000000000000000000000000000000000000000000000000'
           / h'304000000000000000000000000000000000000000000000000000000044000000000000000000000000000000000000000000000000000000'
           / h'405000000000000000000000000000000000000000000000000000000087680203'
           / h'506000000000000000000000000000000000000000000000000000000087680203'
           / h'6070000000000000000000000000000000000000000000000000000000'
           / h'7080000000000000000000000000000000000000000000000000000000'

anchor = [anchor_url : url, anchor_data_hash : $hash32]

asset_name = bytes .size (0 .. 32)

;               metadata: shelley
;   transaction_metadata: shelley-ma
; #6.259(0 ==> metadata): alonzo onwards
; 
auxiliary_data = metadata
                  / [transaction_metadata : metadata
                    , auxiliary_scripts : [* native_script]]
                  / #6.259({? 0 : metadata
                           , ? 1 : [* native_script]
                           , ? 2 : [* plutus_v1_script]
                           , ? 3 : [* plutus_v2_script]
                           , ? 4 : [* plutus_v3_script]})

auxiliary_data_hash = $hash32

babbage_transaction_output = {0 : address
                             , 1 : value
                             , ? 2 : datum_option
                             , ? 3 : script_ref}

big_int = int / big_uint / big_nint

big_nint = #6.3(bounded_bytes)

big_uint = #6.2(bounded_bytes)

; Valid blocks must also satisfy the following two constraints:
; 1. the length of transaction_bodies and transaction_witness_sets
;    must be the same
; 2. every transaction_index must be strictly smaller than the
;    length of transaction_bodies
; 
block = [header
        , transaction_bodies : [* transaction_body]
        , transaction_witness_sets : [* transaction_witness_set]
        , auxiliary_data_set : {* transaction_index => auxiliary_data}
        , invalid_transactions : [* transaction_index]]

block_no = uint .size 8

bootstrap_witness = [public_key : $vkey
                    , signature : $signature
                    , chain_code : bytes .size 32
                    , attributes : bytes]

; The real bounded_bytes does not have this limit. it instead has
; a different limit which cannot be expressed in CDDL.
; 
; The limit is as follows:
;  - bytes with a definite-length encoding are limited to size 0..64
;  - for bytes with an indefinite-length CBOR encoding, each chunk is
;    limited to size 0..64
;  ( reminder: in CBOR, the indefinite-length encoding of
;  bytestrings consists of a token #2.31 followed by a sequence
;  of definite-length encoded bytestrings and a stop code )
; 
bounded_bytes = bytes .size (0 .. 64)

certificate = [stake_registration
               // stake_deregistration
               // stake_delegation
               // pool_registration
               // pool_retirement
               // reg_cert
               // unreg_cert
               // vote_deleg_cert
               // stake_vote_deleg_cert
               // stake_reg_deleg_cert
               // vote_reg_deleg_cert
               // stake_vote_reg_deleg_cert
               // auth_committee_hot_cert
               // resign_committee_cold_cert
               // reg_drep_cert
               // unreg_drep_cert
               // update_drep_cert]

certificates = nonempty_set<certificate>

coin = uint

committee_cold_credential = credential

committee_hot_credential = credential

constitution = [anchor, script_hash / nil]

; The format for cost_models is flexible enough to allow adding
; Plutus built-ins and language versions in the future.
; 
; Plutus v1: only 166 integers are used, but more are accepted (and ignored)
; Plutus v2: only 175 integers are used, but more are accepted (and ignored)
; Plutus v3: only 223 integers are used, but more are accepted (and ignored)
; 
; Any 8-bit unsigned number can be used as a key.
; 
cost_models = {? 0 : [* int64]
              , ? 1 : [* int64]
              , ? 2 : [* int64]
              , * 3 .. 255 => [* int64]}

credential = [0, addr_keyhash // 1, script_hash]

data = #6.24(bytes .cbor plutus_data)

datum_option = [0, $hash32 // 1, data]

; A type for distinct values.
; The type parameter must support .size, for example: bytes or uint
; 
distinct_VBytes = bytes .size 8
                   / bytes .size 16
                   / bytes .size 20
                   / bytes .size 24
                   / bytes .size 30
                   / bytes .size 32

dns_name = text .size (0 .. 128)

drep = [0, addr_keyhash // 1, script_hash // 2 // 3]

drep_credential = credential

drep_voting_thresholds = [unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval]

epoch_interval = uint .size 4

epoch_no = uint .size 8

ex_unit_prices = [mem_price : nonnegative_interval
                 , step_price : nonnegative_interval]

ex_units = [mem : uint, steps : uint]

gov_action = [parameter_change_action
              // hard_fork_initiation_action
              // treasury_withdrawals_action
              // no_confidence
              // update_committee
              // new_constitution
              // info_action]

gov_action_id = [transaction_id : $hash32, gov_action_index : uint .size 2]

header = [header_body, body_signature : $kes_signature]

header_body = [block_number : block_no
              , slot : slot_no
              , prev_hash : $hash32 / nil
              , issuer_vkey : $vkey
              , vrf_vkey : $vrf_vkey
              , vrf_result : $vrf_cert
              , block_body_size : uint .size 4
              , block_body_hash : $hash32
              , operational_cert
              , protocol_version]

info_action = 6

int64 = -9223372036854775808 .. 9223372036854775807

ipv4 = bytes .size 4

ipv6 = bytes .size 16

language = 0 / 1 / 2

major_protocol_version = 1 .. 10

metadata = {* transaction_metadatum_label => transaction_metadatum}

metadata_hash = $hash32

mint = multiasset<nonZeroInt64>

native_script = [script_pubkey
                 // script_all
                 // script_any
                 // script_n_of_k
                 // invalid_before
                 // invalid_hereafter]

negInt64 = -9223372036854775808 .. -1

network_id = 0 / 1

nonZeroInt64 = negInt64 / posInt64

nonnegative_interval = #6.30([uint, positive_int])

operational_cert = [hot_vkey : $kes_vkey
                   , sequence_number : uint .size 8
                   , kes_period : uint
                   , sigma : $signature]

plutus_data = constr<plutus_data>
               / {* plutus_data => plutus_data}
               / [* plutus_data]
               / big_int
               / bounded_bytes

; The real type of plutus_v1_script, plutus_v2_script and
; plutus_v3_script is bytes. However, because we enforce
; uniqueness when many scripts are supplied, we need to hack
; around for tests in order to avoid generating duplicates, since
; the cddl tool we use for roundtrip testing doesn't generate
; distinct collections.
; 
plutus_v1_script = distinct_VBytes

plutus_v2_script = distinct_VBytes

plutus_v3_script = distinct_VBytes

policy_hash = script_hash

policy_id = script_hash

pool_keyhash = $hash28

pool_metadata = [url, metadata_hash]

pool_voting_thresholds = [unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval
                         , unit_interval]

port = uint .le 65535

posInt64 = 1 .. 9223372036854775807

positive_coin = 1 .. 18446744073709551615

positive_int = 1 .. 18446744073709551615

potential_languages = 0 .. 255

proposal_procedure = [deposit : coin, reward_account, gov_action, anchor]

proposal_procedures = nonempty_set<proposal_procedure>

;  0: minfee A
;  1: minfee B
;  2: max block body size
;  3: max transaction size
;  4: max block header size
;  5: key deposit
;  6: pool deposit
;  7: maximum epoch
;  8: n_opt: desired number of stake pools
;  9: pool pledge influence
; 10: expansion rate
; 11: treasury growth rate
; 16: min pool cost
; 17: ada per utxo byte
; 18: cost models for script languages
; 19: execution costs
; 20: max tx ex units
; 21: max block ex units
; 22: max value size
; 23: collateral percentage
; 24: max collateral inputs
; 25: pool voting thresholds
; 26: drep voting thresholds
; 27: min committee size
; 28: committee term limit
; 29: governance action validity period
; 30: governance action deposit
; 31: drep deposit
; 32: drep inactivity period
; 33: minfee refscriptcoinsperbyte
; 
protocol_param_update = {? 0 : coin
                        , ? 1 : coin
                        , ? 2 : uint .size 4
                        , ? 3 : uint .size 4
                        , ? 4 : uint .size 2
                        , ? 5 : coin
                        , ? 6 : coin
                        , ? 7 : epoch_interval
                        , ? 8 : uint .size 2
                        , ? 9 : nonnegative_interval
                        , ? 10 : unit_interval
                        , ? 11 : unit_interval
                        , ? 16 : coin
                        , ? 17 : coin
                        , ? 18 : cost_models
                        , ? 19 : ex_unit_prices
                        , ? 20 : ex_units
                        , ? 21 : ex_units
                        , ? 22 : uint .size 4
                        , ? 23 : uint .size 2
                        , ? 24 : uint .size 2
                        , ? 25 : pool_voting_thresholds
                        , ? 26 : drep_voting_thresholds
                        , ? 27 : uint .size 2
                        , ? 28 : epoch_interval
                        , ? 29 : epoch_interval
                        , ? 30 : coin
                        , ? 31 : coin
                        , ? 32 : epoch_interval
                        , ? 33 : nonnegative_interval}

protocol_version = [major_protocol_version, uint]

; 0: spend
; 1: mint
; 2: cert
; 3: reward
; 4: voting
; 5: proposing
; 
redeemer_tag = 0 / 1 / 2 / 3 / 4 / 5

; Flat Array support is included for backwards compatibility and
; will be removed in the next era. It is recommended for tools to
; adopt using a Map instead of Array going forward.
; 
redeemers = [+ [tag : redeemer_tag
               , index : uint .size 4
               , data : plutus_data
               , ex_units : ex_units]]
             / {+ [tag : redeemer_tag
                  , index : uint .size 4] => [data : plutus_data
                                             , ex_units : ex_units]}

relay = [single_host_addr // single_host_name // multi_host_name]

required_signers = nonempty_set<addr_keyhash>

; reward_account = bytes
; 
reward_account = h'E090000000000000000000000000000000000000000000000000000000'
                  / h'F0A0000000000000000000000000000000000000000000000000000000'

script = [0, native_script
          // 1, plutus_v1_script
          // 2, plutus_v2_script
          // 3, plutus_v3_script]

; This is a hash of data which may affect evaluation of a script.
; This data consists of:
;   - The redeemers from the transaction_witness_set (the value of field 5).
;   - The datums from the transaction_witness_set (the value of field 4).
;   - The value in the cost_models map corresponding to the script's language
;     (in field 18 of protocol_param_update.)
; (In the future it may contain additional protocol parameters.)
; 
; Since this data does not exist in contiguous form inside a transaction, it needs
; to be independently constructed by each recipient.
; 
; The bytestring which is hashed is the concatenation of three things:
;   redeemers || datums || language views
; The redeemers are exactly the data present in the transaction witness set.
; Similarly for the datums, if present. If no datums are provided, the middle
; field is omitted (i.e. it is the empty/null bytestring).
; 
; language views CDDL:
; { * language => script_integrity_data }
; 
; This must be encoded canonically, using the same scheme as in
; RFC7049 section 3.9:
;  - Maps, strings, and bytestrings must use a definite-length encoding
;  - Integers must be as small as possible.
;  - The expressions for map length, string length, and bytestring length
;    must be as short as possible.
;  - The keys in the map must be sorted as follows:
;     -  If two keys have different lengths, the shorter one sorts earlier.
;     -  If two keys have the same length, the one with the lower value
;        in (byte-wise) lexical order sorts earlier.
; 
; For PlutusV1 (language id 0), the language view is the following:
;   - the value of cost_models map at key 0 (in other words, the script_integrity_data)
;     is encoded as an indefinite length list and the result is encoded as a bytestring.
;     (our apologies)
;     For example, the script_integrity_data corresponding to the all zero costmodel for V1
;     would be encoded as (in hex):
;     58a89f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ff
;   - the language ID tag is also encoded twice. first as a uint then as
;     a bytestring. (our apologies)
;     Concretely, this means that the language version for V1 is encoded as
;     4100 in hex.
; For PlutusV2 (language id 1), the language view is the following:
;   - the value of cost_models map at key 1 is encoded as an definite length list.
;     For example, the script_integrity_data corresponding to the all zero costmodel for V2
;     would be encoded as (in hex):
;     98af0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;   - the language ID tag is encoded as expected.
;     Concretely, this means that the language version for V2 is encoded as
;     01 in hex.
; For PlutusV3 (language id 2), the language view is the following:
;   - the value of cost_models map at key 2 is encoded as a definite length list.
; 
; Note that each Plutus language represented inside a transaction must have
; a cost model in the cost_models protocol parameter in order to execute,
; regardless of what the script integrity data is.
; 
; Finally, note that in the case that a transaction includes datums but does not
; include the redeemers field, the script data format becomes (in hex):
; [ A0 | datums | A0 ]
; corresponding to a CBOR empty map and an empty map for language view.
; This empty redeeemer case has changed from the previous eras, since default
; representation for redeemers has been changed to a map. Also whenever redeemers are
; supplied either as a map or as an array they must contain at least one element,
; therefore there is no way to override this behavior by providing a custom
; representation for empty redeemers.
; 
script_data_hash = $hash32

; To compute a script hash, note that you must prepend
; a tag to the bytes of the script before hashing.
; The tag is determined by the language.
; The tags in the Conway era are:
;   "\x00" for multisig scripts
;   "\x01" for Plutus V1 scripts
;   "\x02" for Plutus V2 scripts
;   "\x03" for Plutus V3 scripts
; 
script_hash = $hash28

script_ref = #6.24(bytes .cbor script)

; hash32: datum_hash
; 
shelley_transaction_output = [address, amount : value, ? $hash32]

signkeyKES = bytes .size 64

slot_no = uint .size 8

stake_credential = credential

transaction = [transaction_body
              , transaction_witness_set
              , bool
              , auxiliary_data / nil]

transaction_body = {0 : set<transaction_input>
                   , 1 : [* transaction_output]
                   , 2 : coin
                   , ? 3 : slot_no
                   , ? 4 : certificates
                   , ? 5 : withdrawals
                   , ? 7 : auxiliary_data_hash
                   , ? 8 : slot_no
                   , ? 9 : mint
                   , ? 11 : script_data_hash
                   , ? 13 : nonempty_set<transaction_input>
                   , ? 14 : required_signers
                   , ? 15 : network_id
                   , ? 16 : transaction_output
                   , ? 17 : coin
                   , ? 18 : nonempty_set<transaction_input>
                   , ? 19 : voting_procedures
                   , ? 20 : proposal_procedures
                   , ? 21 : coin
                   , ? 22 : positive_coin}

transaction_index = uint .size 2

transaction_input = [transaction_id : $hash32, index : uint .size 2]

transaction_metadatum = {* transaction_metadatum => transaction_metadatum}
                         / [* transaction_metadatum]
                         / int
                         / bytes .size (0 .. 64)
                         / text .size (0 .. 64)

transaction_metadatum_label = uint .size 8

; Both of the Alonzo and Babbage style TxOut formats are equally valid
; and can be used interchangeably
; 
transaction_output = shelley_transaction_output / babbage_transaction_output

transaction_witness_set = {? 0 : nonempty_set<vkeywitness>
                          , ? 1 : nonempty_set<native_script>
                          , ? 2 : nonempty_set<bootstrap_witness>
                          , ? 3 : nonempty_set<plutus_v1_script>
                          , ? 4 : nonempty_set<plutus_data>
                          , ? 5 : redeemers
                          , ? 6 : nonempty_set<plutus_v2_script>
                          , ? 7 : nonempty_set<plutus_v3_script>}

; The real unit_interval is: #6.30([uint, uint])
; 
; A unit interval is a number in the range between 0 and 1, which
; means there are two extra constraints:
;   1. numerator <= denominator
;   2. denominator > 0
; 
; The relation between numerator and denominator can be
; expressed in CDDL, but we have a limitation currently
; (see: https://github.com/input-output-hk/cuddle/issues/30)
; which poses a problem for testing. We need to be able to
; generate random valid data for testing implementation of
; our encoders/decoders. Which means we cannot use the actual
; definition here and we hard code the value to 1/2
; 
unit_interval = #6.30([1, 2])

url = text .size (0 .. 128)

value = coin / [coin, multiasset<positive_coin>]

vkeywitness = [$vkey, $signature]

vote = 0 .. 2

; 0: constitutional committee hot keyhash
; 1: constitutional committee hot script_hash
; 2: drep keyhash
; 3: drep script_hash
; 4: stakingpool keyhash
; 
voter = [0, addr_keyhash
         // 1, script_hash
         // 2, addr_keyhash
         // 3, script_hash
         // 4, addr_keyhash]

voting_procedure = [vote, anchor / nil]

voting_procedures = {+ voter => {+ gov_action_id => voting_procedure}}

vrf_keyhash = $hash32

withdrawals = {+ reward_account => coin}

auth_committee_hot_cert = (14
                          , committee_cold_credential
                          , committee_hot_credential)

hard_fork_initiation_action = (1, gov_action_id / nil, protocol_version)

invalid_before = (4, slot_no)

invalid_hereafter = (5, slot_no)

; dns_name: An SRV DNS record
; 
multi_host_name = (2, dns_name)

new_constitution = (5, gov_action_id / nil, constitution)

no_confidence = (3, gov_action_id / nil)

parameter_change_action = (0
                          , gov_action_id / nil
                          , protocol_param_update
                          , policy_hash / nil)

;         pool_keyhash: operator
;                 coin: pledge
;                 coin: cost
;        unit_interval: margin
;    set<addr_keyhash>: pool_owners
; 
pool_params = (pool_keyhash
              , vrf_keyhash
              , coin
              , coin
              , unit_interval
              , reward_account
              , set<addr_keyhash>
              , [* relay]
              , pool_metadata / nil)

pool_registration = (3, pool_params)

pool_retirement = (4, pool_keyhash, epoch_no)

reg_cert = (7, stake_credential, coin)

reg_drep_cert = (16, drep_credential, coin, anchor / nil)

resign_committee_cold_cert = (15, committee_cold_credential, anchor / nil)

script_all = (1, [* native_script])

script_any = (2, [* native_script])

script_n_of_k = (3, int64, [* native_script])

script_pubkey = (0, addr_keyhash)

single_host_addr = (0, port / nil, ipv4 / nil, ipv6 / nil)

; dns_name: An A or AAAA DNS record
; 
single_host_name = (1, port / nil, dns_name)

stake_delegation = (2, stake_credential, pool_keyhash)

; This will be deprecated in a future era
stake_deregistration = (1, stake_credential)

stake_reg_deleg_cert = (11, stake_credential, pool_keyhash, coin)

; This will be deprecated in a future era
stake_registration = (0, stake_credential)

stake_vote_deleg_cert = (10, stake_credential, pool_keyhash, drep)

stake_vote_reg_deleg_cert = (13, stake_credential, pool_keyhash, drep, coin)

treasury_withdrawals_action = (2, {* reward_account => coin}, policy_hash / nil)

unreg_cert = (8, stake_credential, coin)

unreg_drep_cert = (17, drep_credential, coin)

update_committee = (4
                   , gov_action_id / nil
                   , set<committee_cold_credential>
                   , {* committee_cold_credential => epoch_no}
                   , unit_interval)

update_drep_cert = (18, drep_credential, anchor / nil)

vote_deleg_cert = (9, stake_credential, drep)

vote_reg_deleg_cert = (12, stake_credential, drep, coin)

constr<a0> = #6.121([* a0])
              / #6.122([* a0])
              / #6.123([* a0])
              / #6.124([* a0])
              / #6.125([* a0])
              / #6.126([* a0])
              / #6.127([* a0])
              / #6.102([uint, [* a0]])

multiasset<a0> = {+ policy_id => {+ asset_name => a0}}

nonempty_set<a0> = #6.258([+ a0]) / [+ a0]

set<a0> = #6.258([* a0]) / [* a0]
```


The classes should be named following Scala 3's conventions and should be placed in the `scalus.ledger` package.

Use scala BigInt for the big_int type.

Use scalus.ledger.api.Timelock for NativeScript type.
Use scalus.builtin.Data for the plutus_data type.

The `ByteString` class should be used to represent the `bytes` type. This class is already provided in the `scalus.builtin` package.
Here is its definition:

```scala
class ByteString private (val bytes: Array[Byte]) {
    def apply(i: Int): Byte = bytes(i)

    override def toString: String = "\"" + toHex + "\""

    override def hashCode: Int = java.util.Arrays.hashCode(bytes)

    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
        case _                => false
    }

    lazy val toHex: String = Hex.bytesToHex(bytes)

    def toBinaryString: String = bytes.view
        .map(b => String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0'))
        .mkString("")

    /** Concatenates two ByteStrings and returns a new ByteString */
    @targetName("concat")
    infix def ++(that: ByteString): ByteString = new ByteString(bytes ++ that.bytes)

    /** The length of the ByteString */
    def size: Int = bytes.length

    /** The length of the ByteString */
    def length: Int = bytes.length

    def isEmpty: Boolean = bytes.isEmpty
}

object ByteString {
    val empty = new ByteString(Array.empty)

    def fromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes.clone)

    def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

    def fill(size: Int, byte: Byte): ByteString =
        val result = new Array[Byte](size)
        if byte != 0 then java.util.Arrays.fill(result, byte)
        new ByteString(result)

    def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)
    def fromHex(bytes: String): ByteString = new ByteString(Hex.hexToBytes(bytes))
    def fromString(s: String): ByteString = new ByteString(s.getBytes("UTF-8"))

    extension (sc: StringContext)
        /** Hex string interpolator
          *
          * @example
          *   {{{
          * val hexString = hex"deadbeef"
          * val withSpaces = hex"de ad be ef"
          * val upperCase = hex"DEADBEEF"
          *   }}}
          */
        def hex(args: Any*): ByteString =
            val hexString = sc.s(args*).replace(" ", "")
            fromHex(hexString)
}
```

I want you to implement CBOR serialization and deserialization for all the classes. 
Use Scala `borer` library for this purpose.
Use borer-derivation as much as possible to derive the encoders and decoders for the classes.
Only write custom encoders and decoders for the classes that cannot be derived using borer-derivation.
Here is the `Encoder` and `Decoder` traits that you should use from the `borer` library:

```scala
package io.bullet.borer
trait Encoder[T]:
  def write(w: Writer, value: T): Writer
  
trait Decoder[T]:
  def read(r: Reader): T
```

Now, generate ONLY the TransactionBody class and all the dependent classes. Write the serialization and deserialization logic for all the classes using the `borer` library.
Make sure you use `writeLong` for the `Long` types.

Please, generate the classes in the order of their dependencies from less dependent to more dependent.
Generate one class at a time and provide the generated code for review before moving to the next class.

Generate ONLY AuxilaryData class.
