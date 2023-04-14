#!/bin/bash

USER="$1"
PAYMENT_UTXO="$2"
RECIPIENT_PKH="$3"
POSIX_TIME="$4"

ADA="4"
AMOUNT_LOVELACE=$(($ADA*1000000))
assets=assets
keypath=keys

NETWORK="--testnet-magic 2"
SENDER_SIGNING_KEY="keys/$USER.skey"
UNSIGNED_OUTPUT="$assets/pv-$RECIPIENT_PKH-$POSIX_TIME.raw"
SIGNED_OUTPUT="$assets/pv-$RECIPIENT_PKH-$POSIX_TIME.signed"

# Build thirtyfivetyped address 
cardano-cli address build \
    --payment-script-file "$assets/pv-$RECIPIENT_PKH-$POSIX_TIME.plutus" \
    $NETWORK \
    --out-file "$assets/pv-$RECIPIENT_PKH-$POSIX_TIME.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    $NETWORK \
    --tx-in "$PAYMENT_UTXO" \
    --tx-out "$(cat "$assets/pv-$RECIPIENT_PKH-$POSIX_TIME.addr") + $AMOUNT_LOVELACE" \
    --tx-out-inline-datum-file "$assets/unit.json" \
    --change-address "$(cat "$keypath/$USER.addr")" \
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