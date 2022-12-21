address=01
netId=1
clinft="85d41568c4585589d4e5d0ed8cdc891c5237e4d73dd940acaf87ce94.4c6f636b4e4654"
nftToken=$(echo -n "LockNFT" | xxd -ps | tr -d '\n')
nftPolicy="cardano-cli transaction policyid --script-file nft-mint-V2.plutus"
socketPath="export CARDANO_NODE_SOCKET_PATH=~/Cardano/cardano-testnet/db-preprod/node.socket"
genKeys="cardano-cli address key-gen --verification-key-file test-addresses/$address.vkey --signing-key-file test-addresses/$address.skey"
querynet="cardano-cli query tip --testnet-magic $netId"
buildAddr="cardano-cli address build --payment-verification-key-file test-addresses/$address.vkey --testnet-magic $netId --out-file test-addresses/$address.addr"
queryUtxo="cardano-cli query utxo --address $(cat test-addresses/$address.addr) --testnet-magic $netId"
