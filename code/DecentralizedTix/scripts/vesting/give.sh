#!/bin/bash

USER=$1
PAYMENT_UTXO=$2
TICKET_NFT_UTXO=$3
EVENT_NFT_UTXO=$4
EVENT_MINT_UTXO=$5
ARTIST=$6
START_TIME=$7
SEAT=$8


ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
ARTIST_NAME=$(echo -n $ARTIST | xxd -ps | tr -d '\n')
EVENT_TOKEN_NAME=$(echo -n "Event" | xxd -ps | tr -d '\n')
TICKET_TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
USER_ADDR=$(cat keys/$USER.addr)
COMPANY_PKH=$(cat keys/company.pkh)
USER_SIGNING_KEY="keys/$USER.skey"

# file outputs
EVENT_POLICY_ID=$(cat "policy/event/$ARTIST_NAME-$START_TIME-$EVENT_MINT_UTXO-$EVENT_TOKEN_NAME")
PARAMS_STRING="$SEAT-$EVENT_POLICY_ID-$EVENT_TOKEN_NAME-$EVENT_NFT_UTXO-$TICKET_TOKEN_NAME"
NETWORK="--testnet-magic 2"
TICKET_POLICY_ID="policy/ticket/$PARAMS_STRING"
USER_POLICY_ID=$(cat "policy/user/$COMPANY_PKH")
USER_TOKEN_NAME=$(echo -n "User" | xxd -ps | tr -d '\n')

# SCRIPT_PARAMS_STRING="$USER_POLICY_ID-$USER_TOKEN_NAME-$COMPANY_PKH-$DEADLINE"
SCRIPT_PARAMS_STRING="$USER_POLICY_ID-$USER_TOKEN_NAME"
SCRIPT_ADDR="assets/vesting/$SCRIPT_PARAMS_STRING.addr"
SIGNED_OUTPUT="assets/vesting/give-tx-$SCRIPT_PARAMS_STRING.signed"
UNSIGNED_OUTPUT="assets/vesting/give-tx-$SCRIPT_PARAMS_STRING.raw"

# Build gift address 
cardano-cli address build \
  --payment-script-file assets/vesting/$SCRIPT_PARAMS_STRING.plutus \
  --testnet-magic 2 \
  --out-file $SCRIPT_ADDR

# Build the transaction
cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $PAYMENT_UTXO \
  --tx-in $TICKET_NFT_UTXO \
  --tx-out "$(cat $SCRIPT_ADDR)+$AMOUNT_LOVELACE + 1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME" \
  --tx-out-inline-datum-file "assets/json/purchase.json" \
  --change-address $USER_ADDR \
  --out-file $UNSIGNED_OUTPUT

# Sign the transaction
cardano-cli transaction sign \
  --tx-body-file $UNSIGNED_OUTPUT \
  --signing-key-file $USER_SIGNING_KEY \
  $NETWORK \
  --out-file $SIGNED_OUTPUT

# Submit the transaction
cardano-cli transaction submit \
  $NETWORK \
  --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"