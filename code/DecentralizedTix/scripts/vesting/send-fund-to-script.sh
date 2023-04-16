#!/bin/bash

USER=$1
PAYMENT_UTXO=$2
TICKET_NFT_UTXO=$3
MINT_UTXO=$4


ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
SCRIPT_ADDR="assets/gift.addr"
SENDER_ADDR=$(cat keys/$USER.addr)
TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
POLICY_ID="policy/ticket-nft-$MINT_UTXO-$TOKEN_NAME"

NETWORK="--testnet-magic 2"
SENDER_SIGNING_KEY="keys/$USER.skey"
UNSIGNED_OUTPUT="assets/pv-$RECIPIENT_PKH-$POSIX_TIME.raw"
SIGNED_OUTPUT="assets/pv-$RECIPIENT_PKH-$POSIX_TIME.signed"

# Build gift address 
cardano-cli address build \
    --payment-script-file assets/parameterized-vesting.plutus \
    --testnet-magic 2 \
    --out-file $SCRIPT_ADDR


# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    $NETWORK \
    --tx-in $PAYMENT_UTXO \
    --tx-in $TICKET_NFT_UTXO \
    --tx-out "$(cat $SCRIPT_ADDR)+$AMOUNT_LOVELACE + 1 $(cat $POLICY_ID).$TOKEN_NAME" \
    --tx-out-inline-datum-file assets/unit.json \
    --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 0 $(cat $POLICY_ID).$TOKEN_NAME" \
    --change-address $SENDER_ADDR \
    --out-file $UNSIGNED_OUTPUT

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