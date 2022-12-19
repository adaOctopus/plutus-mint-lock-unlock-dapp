### CARDANO-CLI APPROACH

Before we build the DAPP on the browser, we need to make sure that we can run the scenarios with bash scripts with `cardano-cli`

We first build a transaction that
-> Locks 100 ADA to the LOckScript contract
-> WIthin that transaction Mint the identification token and send it to the person who locked the funds
-> THen construct another transaction that mints the utility tokens based on the users inputs before to the oock script


#### Plutus V2 approach

Because we love practice, and this is to also help the community to grow we will demonstrate a V2 approach by creating reference scripts for the contracts we built.
