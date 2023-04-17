#!/bin/bash

# params
USER=$1
PAYMENT_UTXO=$2
COLLATERAL=$3
TC_NFT_UTXO=$4
TC_MINT_UTXO=$5
DEADLINE=$6


ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
COMPANY_PKH=$(cat keys/company.pkh)
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
USER_ADDR=$(cat keys/$USER.addr)
USER_PKH=$(cat keys/$USER.pkh)
USER_SIGNING_KEY="keys/$USER.skey"
TC_TOKEN_NAME=$(echo -n "Ticket Creator" | xxd -ps | tr -d '\n')
TICKET_TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
UNIT_JSON="assets/unit.json"

# file outputs
TC_POLICY_ID=$(cat "policy/tc-nft-$COMPANY_PKH-$TC_MINT_UTXO-$TC_TOKEN_NAME")
PARAMS_STRING="$TC_POLICY_ID-$TC_TOKEN_NAME-$PAYMENT_UTXO-$TICKET_TOKEN_NAME"
MINT_SCRIPT="assets/ticket-nft-$PARAMS_STRING.plutus"
TICKET_POLICY_ID="policy/ticket-nft-$PARAMS_STRING"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/ticket-mint-tx-$PARAMS_STRING.signed"
UNSIGNED_OUTPUT="assets/ticket-mint-tx-$PARAMS_STRING.raw"
USER_POLICY_ID=$(cat "policy/user-$COMPANY_PKH")
USER_TOKEN_NAME=$(echo -n "User" | xxd -ps | tr -d '\n')

SCRIPT_PARAMS_STRING="$USER_POLICY_ID-$USER_TOKEN_NAME-$COMPANY_PKH-$DEADLINE"
SCRIPT_ADDR="assets/gift-$SCRIPT_PARAMS_STRING.addr"

# generate protocol params every time the script is run
cardano-cli query protocol-parameters $NETWORK --out-file $PROTOCOL_PARAMS
echo -e "\nprotocol params file $PROTOCOL_PARAMS created\n"

if [ ! -d "policy" ]; then
    mkdir policy
fi

# generate policy id every time the script is run
cardano-cli transaction policyid --script-file $MINT_SCRIPT > $TICKET_POLICY_ID
echo -e "policy id file $TICKET_POLICY_ID created\n"

cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $PAYMENT_UTXO \
  --tx-in $TC_NFT_UTXO \
  --required-signer-hash $USER_PKH \
  --tx-in-collateral $COLLATERAL \
  --tx-out "$(cat $SCRIPT_ADDR)+$AMOUNT_LOVELACE + 1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME" \
  --tx-out-inline-datum-file "assets/unit.json" \
  --tx-out "$USER_ADDR+$AMOUNT_LOVELACE + 1 $TC_POLICY_ID.$TC_TOKEN_NAME" \
  --change-address $USER_ADDR \
  --mint "1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME" \
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