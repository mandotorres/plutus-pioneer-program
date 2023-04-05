#!/bin/bash

# params
# $1 = user

if [ -z "$1" ]; then
  >&2 echo "expected name as argument"
  exit 1
fi

path=/workspace/code/DecentralizedTix/keys
mkdir -p "$path"

vkey="$path/$1.vkey"
skey="$path/$1.skey"
addr="$path/$1.addr"
pkh="$path/$1.pkh"


if [ -f "$vkey" ]; then
  >&2 echo "verification key file $vkey already exists"
  exit 1
fi

if [ -f "$skey" ]; then
  >&2 echo "signing key file $skey already exists"
  exit 1
fi

if [ -f "$addr" ]; then
  >&2 echo "address file $addr already exists"
  exit 1
fi

if [ -f "$pkh" ]; then
  >&2 echo "public key hash file $pkh already exists"
  exit 1
fi

cardano-cli address key-gen --verification-key-file "$vkey" --signing-key-file "$skey" &&
cardano-cli address build --payment-verification-key-file "$vkey" --testnet-magic 2 --out-file "$addr"
cardano-cli address key-hash --payment-verification-key-file "$vkey" --out-file "$pkh"

echo "wrote verification key to: $vkey"
echo "wrote signing key to: $skey"
echo "wrote address to: $addr"
echo "wrote public key hash to: $pkh"