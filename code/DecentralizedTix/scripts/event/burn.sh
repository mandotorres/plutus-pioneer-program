#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
NFT_UTXO=$3 # utxo to consume (contains nft)
COLLATERAL=$4
MINT_UTXO=$5
ARTIST=$6
START_TIME=$7


ADA="3"
AMOUNT_LOVELACE=$(($ADA*1000000))
ARTIST_NAME=$(echo -n $ARTIST | xxd -ps | tr -d '\n')
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
REDEEMER="assets/json/burn.json"
USER_ADDR=$(cat keys/$USER.addr)
USER_PKH=$(cat keys/$USER.pkh)
USER_SIGNING_KEY="keys/$USER.skey"
TOKEN_NAME=$(echo -n "Event" | xxd -ps | tr -d '\n')

PARAMS_STRING="$ARTIST_NAME-$START_TIME-$MINT_UTXO-$TOKEN_NAME"

# file outputs
MINT_SCRIPT="assets/event/$PARAMS_STRING.plutus"
POLICY_ID="policy/event/$PARAMS_STRING"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/event/burn-tx-$PARAMS_STRING.signed"
UNSIGNED_OUTPUT="assets/event/burn-tx-$PARAMS_STRING.raw"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $NFT_UTXO \
  --tx-in $PAYMENT_UTXO \
  --required-signer-hash $USER_PKH \
  --required-signer $USER_SIGNING_KEY \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$USER_ADDR+$AMOUNT_LOVELACE + 0 $(cat $POLICY_ID).$TOKEN_NAME" \
  --change-address $USER_ADDR \
  --mint="-1 $(cat $POLICY_ID).$TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $REDEEMER \
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
#   --tx-body-file $UNSIGNED_OUTPUT \
#   --signing-key-file $INCORRECT_SIGNING_KEY \
#   $NETWORK \
#   --out-file $SIGNED_OUTPUT

cardano-cli transaction submit \
  $NETWORK \
  --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"
