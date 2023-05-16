#!/bin/bash

USER=$1
PAYMENT_UTXO=$2
VESTED_UTXO=$3
USER_TOKEN_UTXO=$4
COLLATERAL=$5
SLOT=$6
TICKET_MINT_UTXO=$7
EVENT_NFT_UTXO=$8
EVENT_MINT_UTXO=$9
ARTIST=${10}
START_TIME=${11}
SEAT=${12}

ADA="2"
AMOUNT_LOVELACE=$(($ADA*1000000))
ARTIST_NAME=$(echo -n $ARTIST | xxd -ps | tr -d '\n')
BENEFICIARY=decentralized-tix
BENEFICIARY_ADDR=$(cat keys/$BENEFICIARY.addr)
BENEFICIARY_PKH=$(cat keys/$BENEFICIARY.pkh)
BENEFICIARY_SIGNING_KEY=keys/$BENEFICIARY.skey
COMPANY_PKH=$(cat keys/company.pkh)
COST=5000000 # 5 ₳ (ada)
PROTOCOL_PARAMS=assets/protocol.params
EVENT_TOKEN_NAME=$(echo -n "Event" | xxd -ps | tr -d '\n')
TICKET_TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
EVENT_POLICY_ID=$(cat "policy/event/$ARTIST_NAME-$START_TIME-$EVENT_MINT_UTXO-$EVENT_TOKEN_NAME")
PARAMS_STRING="$EVENT_POLICY_ID-$EVENT_TOKEN_NAME-$EVENT_NFT_UTXO-$TICKET_TOKEN_NAME"
TICKET_POLICY_ID="policy/ticket/$SEAT-$EVENT_POLICY_ID-$EVENT_TOKEN_NAME-$TICKET_MINT_UTXO-$TICKET_TOKEN_NAME"
USER_POLICY_ID="policy/user/$COMPANY_PKH"
USER_TOKEN_NAME=$(echo -n "User" | xxd -ps | tr -d '\n')
USER_ADDRESS=$(cat keys/$USER.addr)

SCRIPT_PARAMS_STRING="$(cat $USER_POLICY_ID)-$USER_TOKEN_NAME"
SIGNED_OUTPUT=assets/vesting/purchase-tx-$SCRIPT_PARAMS_STRING.signed
UNSIGNED_OUTPUT=assets/vesting/purchase-tx-$SCRIPT_PARAMS_STRING.raw

# Query the protocol parameters \
cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file $PROTOCOL_PARAMS

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --required-signer-hash $BENEFICIARY_PKH \
    --tx-in $PAYMENT_UTXO \
    --tx-in $USER_TOKEN_UTXO \
    --tx-in $VESTED_UTXO \
    --tx-in-script-file assets/vesting/$(cat $USER_POLICY_ID)-$USER_TOKEN_NAME.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file assets/json/unit.json \
    --tx-in-collateral $COLLATERAL \
    --tx-out "$BENEFICIARY_ADDR + $COST" \
    --tx-out "$USER_ADDRESS + $AMOUNT_LOVELACE + 1 $(cat $USER_POLICY_ID).$USER_TOKEN_NAME" \
    --tx-out "$USER_ADDRESS + $AMOUNT_LOVELACE + 1 $(cat $TICKET_POLICY_ID).$TICKET_TOKEN_NAME" \
    --invalid-before $SLOT \
    --change-address $USER_ADDRESS \
    --protocol-params-file $PROTOCOL_PARAMS \
    --out-file $UNSIGNED_OUTPUT

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file $UNSIGNED_OUTPUT \
    --signing-key-file $BENEFICIARY_SIGNING_KEY \
    --signing-key-file keys/$USER.skey \
    --testnet-magic 2 \
    --out-file $SIGNED_OUTPUT

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"