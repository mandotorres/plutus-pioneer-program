#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
UTXO_NFT=$3 # utxo to consume (contains nft)
COLLATERAL=$4
MINT_UTXO=$5
TOKEN_NAME=$(echo -n "$6" | xxd -ps | tr -d '\n')


ADA="3"
AMOUNT_LOVELACE=$(($ADA*1000000))
COLLATERAL_PKH=$(cat keys/$USER.pkh)
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
REDEEMER="assets/unit.json"
SENDER_ADDR=$(cat keys/$USER.addr)
SENDER_SIGNING_KEY="keys/$USER.skey"

# file outputs
MINT_SCRIPT="assets/tc-nft-$MINT_UTXO-$TOKEN_NAME.plutus"
POLICY_ID="policy/tc-nft-$MINT_UTXO-$TOKEN_NAME"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/tc-burn-tx-$MINT_UTXO-$TOKEN_NAME.signed"
UNSIGNED_OUTPUT="assets/tc-burn-tx-$MINT_UTXO-$TOKEN_NAME.raw"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $UTXO_NFT \
  --tx-in $PAYMENT_UTXO \
  --required-signer-hash $COLLATERAL_PKH \
  --required-signer $SENDER_SIGNING_KEY \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 0 $(cat $POLICY_ID).$TOKEN_NAME" \
  --change-address $SENDER_ADDR \
  --mint="-1 $(cat $POLICY_ID).$TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $REDEEMER \
  --protocol-params-file $PROTOCOL_PARAMS \
  --witness-override 2 \
  --out-file $UNSIGNED_OUTPUT

cardano-cli transaction sign \
  --tx-body-file $UNSIGNED_OUTPUT \
  --signing-key-file $SENDER_SIGNING_KEY \
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
