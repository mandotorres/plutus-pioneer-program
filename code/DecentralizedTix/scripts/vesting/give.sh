#!/bin/bash

USER=$1
PAYMENT_UTXO=$2

UNSIGNED_OUTPUT=assets/gift.raw
SIGNED_OUTPUT=assets/gift.signed

# Build gift address 
cardano-cli address build \
    --payment-script-file assets/parameterized-vesting.plutus \
    --testnet-magic 2 \
    --out-file assets/gift.addr

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in $PAYMENT_UTXO \
    --tx-out "$(cat assets/gift.addr) + 2500000 lovelace" \
    --tx-out-inline-datum-file assets/unit.json \
    --change-address $(cat keys/$USER.addr) \
    --out-file $UNSIGNED_OUTPUT
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file $UNSIGNED_OUTPUT \
    --signing-key-file keys/$USER.skey \
    --testnet-magic 2 \
    --out-file $SIGNED_OUTPUT

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file $SIGNED_OUTPUT

TX_ID=$(cardano-cli transaction txid --tx-file $SIGNED_OUTPUT)
echo -e "\nTransaction txid: https://preview.cexplorer.io/tx/$TX_ID\n"