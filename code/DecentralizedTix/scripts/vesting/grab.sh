#!/bin/bash

USER=$1
COLLATERAL=$2
VESTED_UTXO=$3
SLOT=$4
PAYMENT_UTXO=$5
USER_TOKEN_UTXO=$6

BENEFICIARY=company
BENEFICIARY_ADDR=$(cat keys/$BENEFICIARY.addr)
BENEFICIARY_PKH=$(cat keys/$BENEFICIARY.pkh)
BENEFICIARY_SIGNING_KEY=keys/$BENEFICIARY.skey
COST=6000000 # 6 ada
POLICY_ID="policy/user-$BENEFICIARY_PKH"
PROTOCOL_PARAMS=assets/protocol.params
SIGNED_OUTPUT=assets/collect-gift.signed
TOKEN_NAME=$(echo -n "User" | xxd -ps | tr -d '\n')
UNSIGNED_OUTPUT=assets/collect-gift.raw
USER_ADDRESS=$(cat keys/$USER.addr)

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file $PROTOCOL_PARAMS

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --required-signer-hash $BENEFICIARY_PKH \
    --tx-in $PAYMENT_UTXO \
    --tx-in $USER_TOKEN_UTXO \
    --tx-in $VESTED_UTXO \
    --tx-in-script-file assets/parameterized-vesting.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file assets/unit.json \
    --tx-in-collateral $COLLATERAL \
    --tx-out "$BENEFICIARY_ADDR + $COST" \
    --tx-out "$USER_ADDRESS + 2000000 + 1 $(cat $POLICY_ID).$TOKEN_NAME " \
    --invalid-before $SLOT \
    --change-address $USER_ADDRESS \
    --protocol-params-file $PROTOCOL_PARAMS \
    --out-file $UNSIGNED_OUTPUT
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file $UNSIGNED_OUTPUT \
    --signing-key-file $BENEFICIARY_SIGNING_KEY \
    --signing-key-file keys/$USER.skey \
    --testnet-magic 2 \
    --out-file $SIGNED_OUTPUT

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"