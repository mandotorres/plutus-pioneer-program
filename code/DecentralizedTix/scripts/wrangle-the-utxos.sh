cardano-cli transaction build \
  --babbage-era \
  --tx-in 1e711ed633ad90dd85b56570b30c5612fe35ab5358495cf03c1abeb59656ba3e#0 \
  --tx-in 1e711ed633ad90dd85b56570b30c5612fe35ab5358495cf03c1abeb59656ba3e#1 \
  --tx-in 4f1d88d0530d0c22641a3860f765fe0811a8f0a79d26aaea14577a77c50443a4#0 \
  --tx-in 4fda92b9dafdff18f506ce88fdb2e1db9e311ce0e67c5f5b50740cb09d0a8b6c#0 \
  --tx-in 4fda92b9dafdff18f506ce88fdb2e1db9e311ce0e67c5f5b50740cb09d0a8b6c#1 \
  --tx-in e2b6bddc386eac2cf088cdd0d11102ff71f69455cf7a9cb2ac01a1157fb44227#1 \
  --tx-out "addr_test1vpclg5penkqev7mrt37nspcp0cscjtxhjdm6sks8685whqsvzw2pn+0" \
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