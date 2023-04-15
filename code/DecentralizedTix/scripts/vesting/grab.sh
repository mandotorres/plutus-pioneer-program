#!/bin/bash

USER=$1
COLLATERAL=$2
VESTED_UTXO=$3
SLOT=$4

BENEFICIARY_PKH=$(cat keys/company.pkh)
PROTOCOL_PARAMS=assets/protocol-parameters.json
SIGNED_OUTPUT=assets/collect-gift.signed
UNSIGNED_OUTPUT=assets/collect-gift.raw

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file $PROTOCOL_PARAMS

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --required-signer-hash $BENEFICIARY_PKH \
    --tx-in $VESTED_UTXO \
    --tx-in-script-file assets/parameterized-vesting.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file assets/unit.json \
    --tx-in-collateral $COLLATERAL \
    --invalid-before $SLOT \
    --change-address $(cat keys/$USER.addr) \
    --protocol-params-file $PROTOCOL_PARAMS \
    --out-file $UNSIGNED_OUTPUT
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file $UNSIGNED_OUTPUT \
    --signing-key-file keys/$USER.skey \
    --testnet-magic 2 \
    --out-file $SIGNED_OUTPUT

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"