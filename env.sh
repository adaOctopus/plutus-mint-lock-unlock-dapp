address=01
netId=1
clinft="76e5f937c14564f317ea3fd02a980985bd0d3083dae9e21331d671a0.4c6f636b4e4654"
nftToken=$(echo -n "LockNFT" | xxd -ps | tr -d '\n')
nftPolicy="cardano-cli transaction policyid --script-file nft-mint-V2.plutus"
socketPath="export CARDANO_NODE_SOCKET_PATH=~/Cardano/cardano-testnet/db-preprod/node.socket"
genKeys="cardano-cli address key-gen --verification-key-file test-addresses/$address.vkey --signing-key-file test-addresses/$address.skey"
querynet="cardano-cli query tip --testnet-magic $netId"
buildAddr="cardano-cli address build --payment-verification-key-file test-addresses/$address.vkey --testnet-magic $netId --out-file test-addresses/$address.addr"
queryUtxo="cardano-cli query utxo --address $(cat test-addresses/$address.addr) --testnet-magic $netId"
