address=01
netId=1
socketPath="export CARDANO_NODE_SOCKET_PATH=~/Cardano/cardano-testnet/db-preprod/node.socket"
genKeys="cardano-cli address key-gen --verification-key-file test-addresses/$address.vkey --signing-key-file test-addresses/$address.skey"
querynet="cardano-cli query tip --testnet-magic $netId"
buildAddr="cardano-cli address build --payment-verification-key-file test-addresses/$address.vkey --testnet-magic $netId --out-file test-addresses/$address.addr"
queryUtxo="cardano-cli query utxo --address $(cat test-addresses/$address.addr) --testnet-magic $netId"
