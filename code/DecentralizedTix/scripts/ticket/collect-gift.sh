#!/bin/bash

name="$1"
collateral="$2"
txin="$3"
creationNFT="$4"

pp="assets/protocol.params"
body="assets/collect-thirtyfivetyped.raw"
tx="assets/collect-thirtyfivetyped.signed"

ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
SENDER_ADDR=$(cat keys/$1.addr)
TOKEN_NAME=$(echo -n "$5" | xxd -ps | tr -d '\n')
POLICY_ID="policy/policy-id-$creationNFT-$TOKEN_NAME"

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "assets/thirtyfivetyped.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "assets/value35.json" \
    --tx-in-collateral "$collateral" \
    --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $POLICY_ID).$TOKEN_NAME" \
    --change-address "$(cat "keys/$name.addr")" \
    --protocol-params-file "$pp" \
    --out-file "$body"


# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "keys/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cexplorer: https://preview.cexplorer.io/tx/$tid"