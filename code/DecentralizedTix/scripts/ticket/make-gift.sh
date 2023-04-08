#!/bin/bash

ADA="4"
AMOUNT_LOVELACE=$(($ADA*1000000))
assets=assets
keypath=keys
name="$1"
txin="$2"
NETWORK="--testnet-magic 2"
SENDER_SIGNING_KEY="keys/$1.skey"
UNSIGNED_OUTPUT="$assets/thirtyfivetyped.raw"
SIGNED_OUTPUT="$assets/thirtyfivetyped.signed"

# Build thirtyfivetyped address 
cardano-cli address build \
    --payment-script-file "$assets/thirtyfivetyped.plutus" \
    $NETWORK \
    --out-file "$assets/thirtyfivetyped.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    $NETWORK \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/thirtyfivetyped.addr") + $AMOUNT_LOVELACE" \
    --tx-out-inline-datum-file "$assets/unit.json" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$UNSIGNED_OUTPUT"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$UNSIGNED_OUTPUT" \
    --signing-key-file $SENDER_SIGNING_KEY \
  $NETWORK \
  --out-file $SIGNED_OUTPUT

# Submit the transaction
cardano-cli transaction submit \
    $NETWORK \
    --tx-file $SIGNED_OUTPUT

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"