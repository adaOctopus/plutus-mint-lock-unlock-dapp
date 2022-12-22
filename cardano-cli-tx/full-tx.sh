cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --tx-in bf626698bc0ee2ce3c26a322d60aba945cca878cec8d1eadae76d21bbdc8c9a8#0 \
    --tx-in-collateral bf626698bc0ee2ce3c26a322d60aba945cca878cec8d1eadae76d21bbdc8c9a8#0 \
    --change-address $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr) \
    --tx-out $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/lock-v2.addr)+100000000 \
    --tx-out-datum-embed-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/datum-lock.json \
    --tx-out $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr)+15000000+"666666 4401b4b776fe5ec773341935416418ab4c67cb0fdcf4d953cb02c881.57697a617264546f6b656e + 1 49ede37d91ab5d7ed35b976b248cbbf00f8eec469210b911c241c7f1.4c6f636b4e4654" \
    --mint "666666 4401b4b776fe5ec773341935416418ab4c67cb0fdcf4d953cb02c881.57697a617264546f6b656e + 1 49ede37d91ab5d7ed35b976b248cbbf00f8eec469210b911c241c7f1.4c6f636b4e4654" \
    --mint-script-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/utility-token.plutus \
    --mint-redeemer-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/unit.json \
    --mint-script-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/nft-mint-V2.plutus \
    --mint-redeemer-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/unit.json \
    --required-signer-hash 85d4ddf3ab7b5711afc2324077b49f6c6efd9bd71dd4c63267e13d93 \
    --protocol-params-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/protocol-latest.json \
	--out-file "tx-files/full-tx.body" 

cardano-cli transaction sign \
   --tx-body-file tx-files/full-tx.body \
   --testnet-magic 1 \
   --signing-key-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.skey \
   --out-file tx-files/full-tx.signed

cardano-cli transaction submit --tx-file tx-files/full-tx.signed --testnet-magic 1