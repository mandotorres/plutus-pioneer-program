#!/bin/bash

USER="$1"
PAYMENT_UTXO="$2"
UTXO_NFT="$3"
COLLECTION_UTXO="$4"
COLLATERAL="$5"
SLOT="$6"
RECIPIENT_PKH="$7"
POSIX_TIME="$8"
MINT_UTXO=$9
TOKEN_NAME=$(echo -n "$10" | xxd -ps | tr -d '\n')

echo -n "10: $10"
echo "TOKEN_NAME: $TOKEN_NAME"


pp="assets/protocol.params"
body="assets/collect-pv-$RECIPIENT_PKH-$POSIX_TIME.raw"
tx="assets/collect-pv-$RECIPIENT_PKH-$POSIX_TIME.signed"

ADA=4
PRICE=6
AMOUNT_LOVELACE=$(($ADA*1000000))
PRICE_LOVELACE=$(($PRICE*1000000))
SENDER_ADDR=$(cat keys/$USER.addr)
COMPANY_ADDR=$(cat keys/company.addr)
POLICY_ID="policy/user-nft-$MINT_UTXO-55736572"
UNIT_JSON="assets/unit.json"

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$COLLECTION_UTXO" \
    --tx-in "$PAYMENT_UTXO" \
    --tx-in "$UTXO_NFT" \
    --tx-in-script-file "assets/pv-$RECIPIENT_PKH-$POSIX_TIME.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $UNIT_JSON \
    --tx-in-collateral "$COLLATERAL" \
    --tx-out "$SENDER_ADDR+$AMOUNT_LOVELACE + 1 $(cat $POLICY_ID).55736572" \
    --tx-out "$COMPANY_ADDR+$PRICE_LOVELACE" \
    --invalid-before $SLOT \
    --change-address $SENDER_ADDR \
    --protocol-params-file "$pp" \
    --out-file "$body"


# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "keys/$USER.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cexplorer: https://preview.cexplorer.io/tx/$tid"