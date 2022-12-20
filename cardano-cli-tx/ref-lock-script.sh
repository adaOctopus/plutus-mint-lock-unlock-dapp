cardano-cli transaction build \
	--babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --change-address $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr) \
    --tx-in-collateral 9c8ec6b8e92867383e153d8d6047c70714be12587b178fc34e9168c3d99c7b90#1 \
    --tx-in 9c8ec6b8e92867383e153d8d6047c70714be12587b178fc34e9168c3d99c7b90#1 \
    --tx-out $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/lock-script.addr)+100000000 \
    --tx-out-inline-datum-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/jsons/datum-lock.json \
    --tx-out-reference-script-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/locking-v2.plutus \
    --protocol-params-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/protocols.json \
	--out-file "lock-ref.body"

cardano-cli transaction sign \
  --tx-body-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/cardano-cli-tx/lock-ref.body \
  --testnet-magic 1 \
  --signing-key-file  ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.skey \
  --out-file lock-ref.signed

cardano-cli transaction submit --tx-file lock-ref.signed --testnet-magic 1