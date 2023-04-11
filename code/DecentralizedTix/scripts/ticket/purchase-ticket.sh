#!/bin/bash

name="$1"
collateral="$2"
txin="$3"
pkh="$4"
posixTime="$5"
txin2="$6"
slot="$7"
txin3="$8"

pp="assets/protocol.params"
body="assets/collect-pv-$pkh-$posixTime.raw"
tx="assets/collect-pv-$pkh-$posixTime.signed"

ADA="5"
PRICE=6
AMOUNT_LOVELACE=$(($ADA*1000000))
PRICE_LOVELACE=$(($PRICE*1000000))
SENDER_ADDR=$(cat keys/$1.addr)
COMPANY_ADDR=$(cat keys/company.addr)
TOKEN_NAME=$(echo -n "$10" | xxd -ps | tr -d '\n')
POLICY_ID="policy/user-nft-$9-55736572"

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in "$txin2" \
    --tx-in-script-file "assets/pv-$pkh-$posixTime.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "assets/unit.json" \
    --tx-in-collateral "$collateral" \
    --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $POLICY_ID).55736572" \
    --tx-out "$COMPANY_ADDR+$PRICE_LOVELACE" \
    --invalid-before $slot \
    --change-address $SENDER_ADDR \
    --protocol-params-file "$pp" \
    --out-file "$body"


# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "keys/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cexplorer: https://preview.cexplorer.io/tx/$tid"