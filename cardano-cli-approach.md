### CARDANO-CLI APPROACH

Before we build the DAPP on the browser, we need to make sure that we can run the scenarios with bash scripts with `cardano-cli`

We first build a transaction that
-> Locks 100 ADA to the LOckScript contract
-> WIthin that transaction Mint the identification token and send it to the person who locked the funds
-> THen construct another transaction that mints the utility tokens based on the users inputs before to the oock script


#### Plutus V2 approach

Because we love practice, and this is to also help the community to grow we will demonstrate a V2 approach by creating reference scripts for the contracts we built.


#### Serialise NFT policy parameterized with TxOut Ref & Submitting Transaction that locks funds and mints NFT

1. Get inside cabal repl
2. IMport NFTIdent & Utils
3. Run the following `NFTIdent.writeSerialisedScript  (Utils.unsafeReadTxOutRef $ "710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#1") (Utils.unsafeReadAddress $ "addr_test1wrgzpjxkl3249pfsjgmv7mueautkt28kgx2xhehjqhxanecznac34")`
4. Before running replace "710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#1" with a UTXO from your own address, this will be the unique txout ref to be consumed for the NFT to be minted.
5. Inside the `env.sh` file, the clinft is the policy id and the tokenHex of "LockNFT" token name. You can use that if you dont want to rewrite your own.
6. Estimated transaction fee: Lovelace 362507 -> Transaction successfully submitted. -> COSTS OF TRANSACTION

#### Next STep -> Minting Utility Token