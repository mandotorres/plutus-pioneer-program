#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
UTXO_TOKEN=$3 # utxo to consume (contains nft)
COLLATERAL=$4

ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
COLLATERAL_PKH=$(cat keys/$USER.pkh)
COMPANY_PKH=$(cat keys/company.pkh)
COMPANY_SIGNING_KEY="keys/company.skey"
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
SENDER_ADDR=$(cat keys/$USER.addr)
USER_SIGNING_KEY="keys/$USER.skey"
TOKEN_NAME=$(echo -n "User" | xxd -ps | tr -d '\n')
UNIT_JSON="assets/unit.json"

# file outputs
MINT_SCRIPT="assets/user-$COMPANY_PKH.plutus"
POLICY_ID="policy/user-$COMPANY_PKH"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/user-burn-tx-$COMPANY_PKH.signed"
UNSIGNED_OUTPUT="assets/user-burn-tx-$COMPANY_PKH.raw"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $UTXO_TOKEN \
  --tx-in $PAYMENT_UTXO \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 0 $(cat $POLICY_ID).$TOKEN_NAME" \
  --change-address $SENDER_ADDR \
  --required-signer-hash $COLLATERAL_PKH \
  --required-signer-hash $COMPANY_PKH \
  --mint="-1 $(cat $POLICY_ID).$TOKEN_NAME" \
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
