cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --tx-in-collateral 710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#1 \
    --tx-in 710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#1 \
    --change-address $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr) \
    --tx-out $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/lock-v2.addr)+100000000 \
    --tx-out-datum-embed-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/datum-lock.json \
    --tx-out $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr)+3000000+"1 5bd6937d7f79815a777ad6d504ebc67ef2449081127a3fa7e63d38a9.4c6f636b4e4654" \
    --mint "1 5bd6937d7f79815a777ad6d504ebc67ef2449081127a3fa7e63d38a9.4c6f636b4e4654" \
    --mint-script-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/nft-mint-V2.plutus \
    --mint-redeemer-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/unit.json \
    --protocol-params-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/protocol-latest.json \
	--out-file "lock-funds.body" 

cardano-cli transaction sign \
   --tx-body-file lock-funds.body \
   --testnet-magic 1 \
   --signing-key-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.skey \
   --out-file lock-funds.signed

cardano-cli transaction submit --tx-file lock-funds.signed --testnet-magic 1