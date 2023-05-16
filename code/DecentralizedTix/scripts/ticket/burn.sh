#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
NFT_UTXO=$3 # utxo to consume (contains nft)
COLLATERAL=$4
EVENT_NFT_UTXO=$5
TICKET_MINT_UTXO=$6
EVENT_MINT_UTXO=$7
ARTIST=$8
START_TIME=$9
SEAT=${10}

ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
ARTIST_NAME=$(echo -n $ARTIST | xxd -ps | tr -d '\n')
COLLATERAL_PKH=$(cat keys/$USER.pkh)
COMPANY_PKH=$(cat keys/decentralized-tix.pkh)
COMPANY_SIGNING_KEY="keys/decentralized-tix.skey"
EVENT_TOKEN_NAME=$(echo -n "Event" | xxd -ps | tr -d '\n')
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
TICKET_TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
REDEEMER="assets/json/burn.json"
USER_ADDR=$(cat keys/$USER.addr)
USER_SIGNING_KEY="keys/$USER.skey"

# file outputs
EVENT_POLICY_ID=$(cat "policy/event/$ARTIST_NAME-$START_TIME-$EVENT_MINT_UTXO-$EVENT_TOKEN_NAME")
PARAMS_STRING="$SEAT-$EVENT_POLICY_ID-$EVENT_TOKEN_NAME-$TICKET_MINT_UTXO-$TICKET_TOKEN_NAME"
MINT_SCRIPT="assets/ticket/$PARAMS_STRING.plutus"
TICKET_POLICY_ID=$(cat "policy/ticket/$PARAMS_STRING")

PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/ticket/burn-tx-$PARAMS_STRING.signed"
UNSIGNED_OUTPUT="assets/ticket/burn-tx-$PARAMS_STRING.raw"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $NFT_UTXO \
  --tx-in $PAYMENT_UTXO \
  --tx-in $EVENT_NFT_UTXO \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$USER_ADDR+$AMOUNT_LOVELACE + 0 $TICKET_POLICY_ID.$TICKET_TOKEN_NAME" \
  --tx-out "$USER_ADDR+$AMOUNT_LOVELACE + 1 $EVENT_POLICY_ID.$EVENT_TOKEN_NAME" \
  --change-address $USER_ADDR \
  --required-signer-hash $COLLATERAL_PKH \
  --required-signer-hash $COMPANY_PKH \
  --mint="-1 $TICKET_POLICY_ID.$TICKET_TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $REDEEMER \
  --protocol-params-file $PROTOCOL_PARAMS \
  --witness-override 2 \
  --out-file $UNSIGNED_OUTPUT

cardano-cli transaction sign \
  --tx-body-file $UNSIGNED_OUTPUT \
  --signing-key-file $USER_SIGNING_KEY \
  --signing-key-file $COMPANY_SIGNING_KEY \
  $NETWORK \
  --out-file $SIGNED_OUTPUT

# incorrect signing key
# cardano-cli transaction sign \
#   --tx-body-file $UNSIGNED_OUTPUT \
#   --signing-key-file $INCORRECT_SIGNING_KEY \
#   $NETWORK \
#   --out-file $SIGNED_OUTPUT

cardano-cli transaction submit \
  $NETWORK \
  --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"
