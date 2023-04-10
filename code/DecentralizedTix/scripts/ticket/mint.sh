#!/bin/bash

# params
# $1 = sender
# $2 = token name
# $3 = utxoIn
# $4 = collateral

ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
COLLATERAL_PKH=$(cat keys/$1.pkh)
# INCORRECT_SIGNING_KEY="keys/user2.skey"
NETWORK="--testnet-magic 2"
REDEEMER="assets/unit.json"
SCRIPT_ADDR="assets/thirtyfivetyped.addr"
SENDER_ADDR=$(cat keys/$1.addr)
SENDER_SIGNING_KEY="keys/$1.skey"
TICKET_TOKEN_NAME=$(echo -n "$2" | xxd -ps | tr -d '\n')
TC_TOKEN_NAME=$(echo -n "$5" | xxd -ps | tr -d '\n')

# file outputs
MINT_SCRIPT="assets/ticket-nft-$3-$TICKET_TOKEN_NAME.plutus"
TICKET_POLICY_ID="policy/ticket-nft-$3-$TICKET_TOKEN_NAME"
TC_POLICY_ID="policy/tc-nft-$6-$TC_TOKEN_NAME"
PROTOCOL_PARAMS="assets/protocol.params"
SIGNED_OUTPUT="assets/ticket-mint-tx-$3-$TICKET_TOKEN_NAME.signed"
UNSIGNED_OUTPUT="assets/ticket-mint-tx-$3-$TICKET_TOKEN_NAME.raw"

# generate protocol params every time the script is run
cardano-cli query protocol-parameters $NETWORK --out-file $PROTOCOL_PARAMS
echo -e "\nprotocol params file $PROTOCOL_PARAMS created\n"

if [ ! -d "policy" ]; then
    mkdir policy
fi

# generate policy id every time the script is run
cardano-cli transaction policyid --script-file $MINT_SCRIPT > $TICKET_POLICY_ID
echo -e "policy id file $TICKET_POLICY_ID created\n"

# echo "sender: $1"
# echo "token name: $2"
# echo "utxoIn: $3"
# echo -e "collateral: $4\n"

# echo "ada amount: $ADA"
# echo "lovelace amount: $AMOUNT_LOVELACE"
# echo "collateral pkh: $COLLATERAL_PKH"
# echo "minting script: $MINT_SCRIPT"
# echo "network: $NETWORK"
# echo "policy id: $TICKET_POLICY_ID"
# echo "protocol params: $PROTOCOL_PARAMS"
# echo "redeemer: $REDEEMER"
# echo "sender addr: $SENDER_ADDR"
# echo "sender signing key: $SENDER_SIGNING_KEY"
# echo "signed tx file: $SIGNED_OUTPUT"
# echo "token name: $TICKET_TOKEN_NAME"
# echo -e "unsigned tx file: $UNSIGNED_OUTPUT\n"

# echo "$SENDER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME"
# echo "--tx-out $SENDER_ADDR+$AMOUNT_LOVELACE+1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME"

# --tx-in "8bad8b001474817b3dffd7cdf6022dcc4b0fa34d62412a9cb802f136f4a83a95#1" \
cardano-cli transaction build \
  --babbage-era \
  $NETWORK \
  --tx-in $3 \
  --tx-in $7 \
  --required-signer-hash $COLLATERAL_PKH \
  --tx-in-collateral $4 \
  --tx-out "$(cat $SCRIPT_ADDR)+$AMOUNT_LOVELACE + 1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME" \
  --tx-out-inline-datum-file "assets/unit.json" \
  --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $TC_POLICY_ID).$TC_TOKEN_NAME" \
  --change-address $SENDER_ADDR \
  --mint "1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME" \
  --mint-script-file $MINT_SCRIPT \
  --mint-redeemer-file $REDEEMER \
  --protocol-params-file $PROTOCOL_PARAMS \
  --witness-override 2 \
  --out-file $UNSIGNED_OUTPUT

# cardano-cli transaction sign \
#   --tx-body-file $UNSIGNED_OUTPUT \
#   --signing-key-file $SENDER_SIGNING_KEY \
#   $NETWORK \
#   --out-file $SIGNED_OUTPUT

# # incorrect signing key
# # cardano-cli transaction sign \
# #     --tx-body-file $UNSIGNED_OUTPUT \
# #     --signing-key-file $INCORRECT_SIGNING_KEY \
# #     $NETWORK \
# #     --out-file $SIGNED_OUTPUT

# cardano-cli transaction submit \
#   $NETWORK \
#   --tx-file $SIGNED_OUTPUT

# TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
# echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"