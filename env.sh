address=01
netId=1
clinft="49ede37d91ab5d7ed35b976b248cbbf00f8eec469210b911c241c7f1.4c6f636b4e4654"
utilToken="4401b4b776fe5ec773341935416418ab4c67cb0fdcf4d953cb02c881.57697a617264546f6b656e"
nftToken=$(echo -n "LockNFT" | xxd -ps | tr -d '\n')
utilityToken=$(echo -n "WizardToken" | xxd -ps | tr -d '\n')
nftPolicy="cardano-cli transaction policyid --script-file nft-mint-V2.plutus"
socketPath="export CARDANO_NODE_SOCKET_PATH=~/Cardano/cardano-testnet/db-preprod/node.socket"
genKeys="cardano-cli address key-gen --verification-key-file test-addresses/$address.vkey --signing-key-file test-addresses/$address.skey"
querynet="cardano-cli query tip --testnet-magic $netId"
buildAddr="cardano-cli address build --payment-verification-key-file test-addresses/$address.vkey --testnet-magic $netId --out-file test-addresses/$address.addr"
queryUtxo="cardano-cli query utxo --address $(cat test-addresses/$address.addr) --testnet-magic $netId"
queryScript="cardano-cli query utxo --address $(cat lock-v2.addr) --testnet-magic $netId"
