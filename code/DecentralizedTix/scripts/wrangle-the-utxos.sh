u2Wallet="addr_test1vz3e2dmwn30r2m453pqap9a3hv2c7mnd3nkdexn4dktxuhgw4u4au"
userSymbol="policy/policy-id-8641babaf7c354e1d106a1ea1e6c0330e11b55642a3de32d8ea52cc41829a226#0-5375706572436f696e"
TOKEN_NAME=$(echo -n "SuperCoin" | xxd -ps | tr -d '\n')
scriptAddress="addr_test1wzw3vnt2xnytxwcv7526ju4mgszqkkfgz4szyrk9fzsq8zg8gl4uf"
ticketSymbol="policy/policy-id-4691162ae46b1a7bf80715aa442f43e78031b70520ba17572d02fa6b275c5387#1-5375706572436f696e"

cardano-cli transaction build \
  --babbage-era \
  --tx-in 8641babaf7c354e1d106a1ea1e6c0330e11b55642a3de32d8ea52cc41829a226#1 \
  --tx-in a0c84f13cd94fc0372fcb724145988e7a0a2f17c995dca79920e56127189bd6a#1 \
  --tx-in df0a2f4a9bcdb090acd9d28c600892512b279b1feee9e07662e10f519aa111ff#0 \
  --tx-in df0a2f4a9bcdb090acd9d28c600892512b279b1feee9e07662e10f519aa111ff#1 \
  --tx-out "$u2Wallet + 5000000 + 1 $(cat $userSymbol).$TOKEN_NAME" \
  --tx-out "$scriptAddress + 5000000 + 1 $(cat $ticketSymbol).$TOKEN_NAME" \
  --tx-out-inline-datum-file "assets/unit.json" \
  --change-address $u2Wallet \
  --testnet-magic 2 \
  --out-file assets/fund-tx.raw

cardano-cli transaction sign \
  --tx-body-file assets/fund-tx.raw \
  --signing-key-file keys/user2.skey \
  --testnet-magic 2 \
  --out-file assets/fund-tx.signed

cardano-cli transaction submit \
  --testnet-magic 2 \
  --tx-file assets/fund-tx.signed

cardano-cli transaction txid --tx-file assets/fund-tx.signed