#!/bin/bash

USER=$1
PAYMENT_UTXO=$2
VESTED_UTXO=$3
COLLATERAL=$4

BENEFICIARY=company
BENEFICIARY_PKH=$(cat keys/$BENEFICIARY.pkh)
BENEFICIARY_SIGNING_KEY=keys/$BENEFICIARY.skey
COST=6000000 # 6 ada
PROTOCOL_PARAMS=assets/protocol.params
SIGNED_OUTPUT=assets/collect-gift.signed
TOKEN_NAME=$(echo -n "Ticket" | xxd -ps | tr -d '\n')
POLICY_ID="policy/ticket-nft-922f2c0832091e956b666f64f14d37918a3308e604e2bf99f571e86e59e052ad#1-$TOKEN_NAME" # ticket-nft-922f2c0832091e956b666f64f14d37918a3308e604e2bf99f571e86e59e052ad#1-5469636b6574
UNSIGNED_OUTPUT=assets/collect-gift.raw
USER_ADDRESS=$(cat keys/$USER.addr)

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
    --tx-in $VESTED_UTXO \
    --tx-in-script-file assets/thirtyfivetyped.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file assets/value35.json \
    --tx-in-collateral $COLLATERAL \
    --tx-out "$USER_ADDRESS + 2000000 + 1 $(cat $POLICY_ID).$TOKEN_NAME " \
    --change-address $USER_ADDRESS \
    --protocol-params-file $PROTOCOL_PARAMS \
    --out-file $UNSIGNED_OUTPUT
    
echo "asset class: $(cat $POLICY_ID).$TOKEN_NAME"

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