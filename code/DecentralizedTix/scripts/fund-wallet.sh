#!/bin/bash

# params
# $1 = sender
# $2 = utxoIn
# $3 = amount in ada
# $4 = receiver

LOVELACE=$(($3*1000000))
NETWORK="--testnet-magic 2"
OUTPUT=20000000
RECEIVER_ADDR=$(cat keys/$4.addr)
SENDER_ADDR=$(cat keys/$1.addr)
SIGNED_OUTPUT="assets/fund-tx.signed"
SIGNING_KEY="keys/$1.skey"
UNSIGNED_OUTPUT="assets/fund-tx.raw"

echo -e "\nsender: $1"
echo "utxoIn: $2"
echo "amount in ada: $3"
echo -e "receiver: $4\n"

echo "amount in lovelace: $LOVELACE"
echo "network: $NETWORK"
echo "output amount: $OUTPUT"
echo "receiver addr: $RECEIVER_ADDR"
echo "sender addr: $SENDER_ADDR"
echo "signed tx file: $SIGNED_OUTPUT"
echo "signing key: $SIGNING_KEY"
echo -e "unsigned tx file: $UNSIGNED_OUTPUT\n"

# gather funds into user 1 wallet
# cardano-cli transaction build \
#   --babbage-era \
#   --tx-in $2 \
#   --tx-in $IN \
#   --tx-out $RECEIVER_ADDR+$LOVELACE \
#   --change-address $RECEIVER_ADDR \
#   $NETWORK \
#   --out-file $UNSIGNED_OUTPUT

# transfer funds
cardano-cli transaction build \
  --babbage-era \
  --tx-in $2 \
  --tx-out $RECEIVER_ADDR+$LOVELACE \
  --change-address $SENDER_ADDR \
  $NETWORK \
  --out-file $UNSIGNED_OUTPUT

# transfer funds, reciever keeps change
# cardano-cli transaction build \
#   --babbage-era \
#   --tx-in $2 \
#   --tx-out $RECEIVER_ADDR+$LOVELACE \
#   --change-address $RECEIVER_ADDR \
#   $NETWORK \
#   --out-file $UNSIGNED_OUTPUT

# sign both
# cardano-cli transaction sign \
#   --tx-body-file $UNSIGNED_OUTPUT \
#   --signing-key-file $SIGNING_KEY \
#   --signing-key-file $SIGN \
#   $NETWORK \
#   --out-file $SIGNED_OUTPUT

cardano-cli transaction sign \
  --tx-body-file $UNSIGNED_OUTPUT \
  --signing-key-file $SIGNING_KEY \
  $NETWORK \
  --out-file $SIGNED_OUTPUT

cardano-cli transaction submit \
  $NETWORK \
  --tx-file $SIGNED_OUTPUT