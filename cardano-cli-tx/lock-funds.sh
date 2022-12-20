cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --tx-in-collateral 2473cd4fe424a5f2f168ad55f70065d93c4b563addad2b92f60e50eda3be8b20#1 \
    --tx-in 2473cd4fe424a5f2f168ad55f70065d93c4b563addad2b92f60e50eda3be8b20#1 \
    --change-address $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr) \
    --tx-out $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/lock-v2.addr)+100000000 \
    --tx-out-datum-embed-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/datum-lock.json \
    --protocol-params-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/protocols.json \
	--out-file "lock-funds.body" 

cardano-cli transaction sign \
   --tx-body-file lock-funds.body \
   --testnet-magic 1 \
   --signing-key-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.skey \
   --out-file lock-funds.signed

cardano-cli transaction submit --tx-file lock-funds.signed --testnet-magic 1 