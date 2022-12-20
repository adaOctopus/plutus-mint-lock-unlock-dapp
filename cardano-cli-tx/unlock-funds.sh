cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --tx-in 710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#0 \
	--tx-in-script-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/locking-v2.plutus \
	--tx-in-datum-file  ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/datum-lock.json \
	--tx-in-redeemer-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/unlock-redeemer.json \
    --tx-in-collateral 710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#1 \
    --change-address $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr) \
    --required-signer-hash 85d4ddf3ab7b5711afc2324077b49f6c6efd9bd71dd4c63267e13d93 \
    --protocol-params-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/protocols.json \
	--out-file "unlock-funds.body"


cardano-cli transaction sign \
   --tx-body-file unlock-funds.body \
   --testnet-magic 1 \
   --signing-key-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.skey \
   --out-file unlock-funds.signed

cardano-cli transaction submit --tx-file unlock-funds.signed --testnet-magic 1 