### CARDANO-CLI APPROACH

Before we build the DAPP on the browser, we need to make sure that we can run the scenarios with bash scripts with `cardano-cli`

We first build a transaction that
-> Locks 100 ADA to the LOckScript contract
-> WIthin that transaction Mint the identification token and send it to the person who locked the funds
-> THen construct another transaction that mints the utility tokens based on the users inputs before to the oock script


### Note

In order to run this in your own environment, you have to update everytime the NFTIdent serialization, with your own TxRef from your own wallet so it can work.
For the script address though, you can use the one shown below:
addr_test1wrgzpjxkl3249pfsjgmv7mueautkt28kgx2xhehjqhxanecznac34


#### Plutus V2 approach

Because we love practice, and this is to also help the community to grow we will demonstrate a V2 approach by creating reference scripts for the contracts we built.


#### Serialise NFT policy parameterized with TxOut Ref & Submitting Transaction that locks funds and mints NFT

1. Get inside cabal repl
2. IMport NFTIdent & Utils
3. Run the following `NFTIdent.writeSerialisedScript  (Utils.unsafeReadTxOutRef $ "f5f8b29457d6a37a4068bf2b4a497e4fc346043679eb94bc6da84547dd35cbc0#2") (Utils.unsafeReadAddress $ "addr_test1wrgzpjxkl3249pfsjgmv7mueautkt28kgx2xhehjqhxanecznac34")`

// THis is using NAMI wallet (browser case)
`NFTIdent.writeSerialisedScript  (Utils.unsafeReadTxOutRef $ "969798dbfe9af3662d2fb016425792e207f8f440c6926ac58f26b573526968d2#1") (Utils.unsafeReadAddress $ "addr_test1wrgzpjxkl3249pfsjgmv7mueautkt28kgx2xhehjqhxanecznac34")`
4. Before running replace "710cad3dbfae3cc0bbabf7ea5f8f34d9f6cb90f423823e4bc41b674a03751caf#1" with a UTXO from your own address, this will be the unique txout ref to be consumed for the NFT to be minted.
5. Inside the `env.sh` file, the clinft is the policy id and the tokenHex of "LockNFT" token name. You can use that if you dont want to rewrite your own.
6. Estimated transaction fee: Lovelace 362507 -> Transaction successfully submitted. -> COSTS OF TRANSACTION

#### Next STep -> Minting Utility Token

1. Fixed the minting policy for utility token
2. It basically checks 2 things
  a. If the OUTPUT of the lockscript has the correct ratio 1:10 for ada locked
  b. If the OUTPUT of the user has indeed the 1 NFT about to be minted.

import UtilityToken and Utils inside repl
3. To serialize it run `UtilityToken.writeSerialisedScriptV2 (Utils.unsafeReadAddress $ "addr_test1wrgzpjxkl3249pfsjgmv7mueautkt28kgx2xhehjqhxanecznac34")`
4. Replace my address with your testing address.

##### Useful `cabal repl` 

`Plutus.V1.Ledger.Value.singleton (currencySymbol . fromString $ "a8ff") (getTokenName "ABC") 7`
Value (Map [(61386666,Map [("ABC",7)])])

-> This is for you to test different kind of Value structures and how to access them.


### Cardano CLI transaction completed

THe full tx that mints both tokens and locks funds work fine from cardano-cli.
However, doing it from the browser is not done yet.