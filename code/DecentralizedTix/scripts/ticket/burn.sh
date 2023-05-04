#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
NFT_UTXO=$3 # utxo to consume (contains nft)
COLLATERAL=$4
TICKET_MINT_UTXO=$5
EVENT_MINT_UTXO=$6


ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
COLLATERAL_PKH=$(cat keys/$USER.pkh)
COMPANY_PKH=$(cat keys/company.pkh)
COMPANY_SIGNING_KEY="keys/company.skey"
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
USER_ADDR=$(cat keys/$USER.addr)
USER_SIGNING_KEY="keys/$USER.skey"
EVENT_TOKEN_NAME=$(echo -n "Event" | xxd -ps | tr -d '\n')
TICKET_TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
USER_TOKEN_NAME=$(echo -n "User" | xxd -ps | tr -d '\n')
UNIT_JSON="assets/unit.json"

# file outputs
EVENT_POLICY_ID=$(cat "policy/event-nft-$COMPANY_PKH-$EVENT_MINT_UTXO-$EVENT_TOKEN_NAME")
PARAMS_STRING="$EVENT_POLICY_ID-$EVENT_TOKEN_NAME-$TICKET_MINT_UTXO-$TICKET_TOKEN_NAME"
MINT_SCRIPT="assets/ticket-nft-$PARAMS_STRING.plutus"
TICKET_POLICY_ID=$(cat "policy/ticket-nft-$PARAMS_STRING")

PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/ticket-mint-tx-$PARAMS_STRING.signed"
UNSIGNED_OUTPUT="assets/ticket-mint-tx-$PARAMS_STRING.raw"
USER_POLICY_ID=$(cat "policy/user-$COMPANY_PKH")

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $NFT_UTXO \
  --tx-in $PAYMENT_UTXO \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$USER_ADDR+$AMOUNT_LOVELACE + 0 $TICKET_POLICY_ID.$TICKET_TOKEN_NAME" \
  --change-address $USER_ADDR \
  --required-signer-hash $COLLATERAL_PKH \
  --required-signer-hash $COMPANY_PKH \
  --mint="-1 $TICKET_POLICY_ID.$TICKET_TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $UNIT_JSON \
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
