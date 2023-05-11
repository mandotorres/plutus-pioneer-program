#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
COLLATERAL=$3
ARTIST=$4
START_TIME=$5

ADA="3"
AMOUNT_LOVELACE=$(($ADA*1000000))
ARTIST_NAME=$(echo -n $ARTIST | xxd -ps | tr -d '\n')
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
USER_ADDR=$(cat keys/$USER.addr)
USER_PKH=$(cat keys/$USER.pkh)
USER_SIGNING_KEY="keys/$USER.skey"
TOKEN_NAME=$(echo -n "Event" | xxd -ps | tr -d '\n')
UNIT_JSON="assets/json/mint.json"

PARAMS_STRING="$ARTIST_NAME-$START_TIME-$PAYMENT_UTXO-$TOKEN_NAME"

# file outputs
MINT_SCRIPT="assets/event/$PARAMS_STRING.plutus"
POLICY_ID="policy/event/$PARAMS_STRING"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/event/mint-tx-$PARAMS_STRING.signed"
UNSIGNED_OUTPUT="assets/event/mint-tx-$PARAMS_STRING.raw"

# generate protocol params every time the script is run
cardano-cli query protocol-parameters $NETWORK --out-file $PROTOCOL_PARAMS
echo -e "\nprotocol params file $PROTOCOL_PARAMS created\n"

if [ ! -d "policy/event" ]; then
    mkdir -p policy/event
fi

# generate policy id every time the script is run
cardano-cli transaction policyid --script-file $MINT_SCRIPT > $POLICY_ID
echo -e "policy id file $POLICY_ID created\n"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $PAYMENT_UTXO \
  --required-signer-hash $USER_PKH \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$USER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $POLICY_ID).$TOKEN_NAME" \
  --change-address $USER_ADDR \
  --mint "1 $(cat $POLICY_ID).$TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $UNIT_JSON \
  --protocol-params-file $PROTOCOL_PARAMS \
  --witness-override 2 \
  --out-file $UNSIGNED_OUTPUT

cardano-cli transaction sign \
  --tx-body-file $UNSIGNED_OUTPUT \
  --signing-key-file $USER_SIGNING_KEY \
  $NETWORK \
  --out-file $SIGNED_OUTPUT

# incorrect signing key
# cardano-cli transaction sign \
#     --tx-body-file $UNSIGNED_OUTPUT \
#     --signing-key-file $INCORRECT_SIGNING_KEY \
#     $NETWORK \
#     --out-file $SIGNED_OUTPUT

cardano-cli transaction submit \
  $NETWORK \
  --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"