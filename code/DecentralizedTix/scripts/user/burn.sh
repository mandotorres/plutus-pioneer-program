#!/bin/bash

# params
# $1 = sender
# $2 = token name
# $3 = utxo to consume (contains nft)
# $4 = collateral
# $5 = creation utxo
# $6 = extra utxo (not used currently)

ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
COLLATERAL_PKH=$(cat keys/$1.pkh)
NETWORK="--testnet-magic 2"
REDEEMER="assets/unit.json"
SENDER_ADDR=$(cat keys/$1.addr)
SENDER_SIGNING_KEY="keys/$1.skey"
TOKEN_NAME=$(echo -n "$2" | xxd -ps | tr -d '\n')


MINT_SCRIPT="assets/user-nft-$5-$TOKEN_NAME.plutus"
POLICY_ID="policy/policy-id-$5-$TOKEN_NAME"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/user-burn-tx-$5-$TOKEN_NAME.signed"
UNSIGNED_OUTPUT="assets/user-burn-tx-$5-$TOKEN_NAME.raw"


# echo "sender: $1"
# echo "token name: $2"
# echo "utxoIn: $3"
# echo -e "collateral: $4\n"

# echo "ada amount: $ADA"
# echo "lovelace amount: $AMOUNT_LOVELACE"
# echo "collateral pkh: $COLLATERAL_PKH"
# echo "minting script: $MINT_SCRIPT"
# echo "network: $NETWORK"
# echo "policy id: $POLICY_ID"
# echo "protocol params: $PROTOCOL_PARAMS"
# echo "redeemer: $REDEEMER"
# echo "sender addr: $SENDER_ADDR"
# echo "sender signing key: $SENDER_SIGNING_KEY"
# echo "signed tx file: $SIGNED_OUTPUT"
# echo "token name: $TOKEN_NAME"
# echo -e "unsigned tx file: $UNSIGNED_OUTPUT\n"

# echo "$SENDER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $POLICY_ID).$TOKEN_NAME"
# echo "--tx-out $SENDER_ADDR+$AMOUNT_LOVELACE+1 $(cat $POLICY_ID).$TOKEN_NAME"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $3 \
  --required-signer-hash $COLLATERAL_PKH \
  --tx-in-collateral $4 \
  --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 0 $(cat $POLICY_ID).$TOKEN_NAME" \
  --mint="-1 $(cat $POLICY_ID).$TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $REDEEMER \
  --change-address $SENDER_ADDR \
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
