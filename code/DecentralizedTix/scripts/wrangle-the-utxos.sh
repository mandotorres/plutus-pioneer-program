cardano-cli transaction build \
  --babbage-era \
  --tx-in 23e7a596c829c101d9497154038f5e5319619b4b46dd3516b10d01384b0fa1f6#0 \
  --tx-in 23e7a596c829c101d9497154038f5e5319619b4b46dd3516b10d01384b0fa1f6#1 \
  --tx-in 7fc87dac56eb1dbf677353f3443cd3bfabb1c629ba5b2ee03136283c3d02628c#1 \
  --tx-in aa36f882e4e9d1390c5dc22827d9a679318f7efbd3cea1d6e7bb93f5af14cdf7#0 \
  --tx-in e9e7834d317135bc2b6bf463339cf862794d0124b79943150debe016439ff66b#1 \
  --tx-out "addr_test1vpclg5penkqev7mrt37nspcp0cscjtxhjdm6sks8685whqsvzw2pn+1000000000" \
  --change-address addr_test1vpclg5penkqev7mrt37nspcp0cscjtxhjdm6sks8685whqsvzw2pn \
  --testnet-magic 2 \
  --out-file assets/fund-tx.raw

cardano-cli transaction sign \
  --tx-body-file assets/fund-tx.raw \
  --signing-key-file keys/user1.skey \
  --testnet-magic 2 \
  --out-file assets/fund-tx.signed

cardano-cli transaction submit \
  --testnet-magic 2 \
  --tx-file assets/fund-tx.signed

cardano-cli transaction txid --tx-file assets/fund-tx.signed