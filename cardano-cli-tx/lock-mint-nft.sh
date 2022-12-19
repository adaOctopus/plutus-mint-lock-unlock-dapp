cardano-cli transaction build \
	--babbage-era \
        --cardano-mode \
	--testnet-magic 1 \
	--tx-in-collateral 9c8ec6b8e92867383e153d8d6047c70714be12587b178fc34e9168c3d99c7b90#1 \
        --tx-in 9c8ec6b8e92867383e153d8d6047c70714be12587b178fc34e9168c3d99c7b90#1 \
	--change-address $(cat ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.addr) \
