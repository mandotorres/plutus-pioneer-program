#!/bin/bash

name="$1"
txin="$2"
pkh="$3"
posixTime="$4"

ADA="4"
AMOUNT_LOVELACE=$(($ADA*1000000))
assets=assets
keypath=keys

NETWORK="--testnet-magic 2"
SENDER_SIGNING_KEY="keys/$1.skey"
UNSIGNED_OUTPUT="$assets/pv-$pkh-$posixTime.raw"
SIGNED_OUTPUT="$assets/pv-$pkh-$posixTime.signed"

# Build thirtyfivetyped address 
cardano-cli address build \
    --payment-script-file "$assets/pv-$pkh-$posixTime.plutus" \
    $NETWORK \
    --out-file "$assets/pv-$pkh-$posixTime.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    $NETWORK \
    --tx-in "$txin" \
    --tx-out "$(cat "$assets/pv-$pkh-$posixTime.addr") + $AMOUNT_LOVELACE" \
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

tid=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"