# plutus-mint-lock-unlock-dapp
This repository has 3 Plutus Smart contracts to implement the design explained in the ReadME file. 

### Status <br/>
BACKEND </br>
-> LockScript Completed  <br/> 
-> Utility Token Policy -> Completed <br/>
-> NFT Identification Policy -> Pending <br/>
-> Offchain TX Construction with Cardano CLI -> (1) Lock Funds COmpleted, (2) Unlock FUnds pending <br/>
-> Offchain TX Construction with Cardano CLI -> (3) Minting nft while locking COMPLETED, (4) Mint utility token COMPLETED <br/>
-> All `cardano-cli` txs completed under one tx. Locking funds works, minting identity NFT works, minting Utility token works, unlocking funds from script works
-> Contract Monad pending for testing
-> Property based testing pending. AFter finishing the UI

FRONTEND </br>
-> UI with NextJS using Mesh Library to do offchain from browser -> Pending <br/>
-> CHeck this repo for offchain & frontend : https://github.com/tas2017/plutus-dapp-lucid


### FOR HELP JOIN OUR PLUTUS WIZARDS SERVER ON DISCORD: https://discord.gg/hvGtC7Xh


#### Please
Review the HOWTOUSE.md file for how to play with this repo locally.
This current document is an explanation of the concept.

############################################################################

## *Trust No Bank*

### [Assumption]

Alice is concerned about the macroeconomic situation of this world and she does not want to hold her money inside a bank. She says, 'Trust no bank.'
She is a coder, so she believes in the God of Colorful Lines.

Therefore she wants to lock her funds inside a smart contract, and that smart contract to mint some tokens for her to represent her initial fund deposit. In that way she can use those tokens for her every day activities.

However, Alice wants to be able to withdraw all of her funds in case of an emergency.


### [System Design]

In order to achieved Alice's goal based on the above assumption, we need to answer some specific questions.

1) [What is Alice's ultimate goal?]
 - To keep her funds safe
2)  [How does she wish to do that?]
 - Locking her funds in the smart contract
 - Receiving Minted Tokens in exchange
 - Redeem her funds back in full when there is an emergency.

### [Smart Contracts Design]

`Smart Contract #1`

```
This is the contract that locks Alice's funds, with an attached datum, and with a redeemer action so Alice can withdraw the funds whenever she wants
```

`Smart Contract #2`

```
This is the contract that defines the minting policy for the tokens that Alice will receive based on the amount of Ada she deposited to the Lock/Unlock Smart Contract #1
```


We could assume that those 2 Contracts suffice for the system we want to build. But let's ask our selves these questions:

- [How does the minting policy script know, that Alice indeed deposit the specific amount of Ada she claims she did?]

1) *The minting script can check the UTXO under the Lock/Unlock script to see the balance*

- [Yes, but what if a malicious actor comes, and uses the same utxo from Alice's transaction as a reference claimming that it is his/her funds, and wants to mint tokens out of thin air??]

2) *An addition NFT Minting Policy Script will be implemented in order to validate and identify that indeed Alice (and whoever wants to Mint Tokens) has deposited the initial fund and she can mint the Tokens.*

`Smart Contract #3`

```
NFT Minting Script that will mint a unique identifier token and send it to Alice's address when alice deposits the initial amount of funds in the lock/unlock Script

```

In that way noone else can claim that owns the initial funds to mint and unlimited amount of Tokens.

PS: Maybe there are still risks and loopholes in that design, but we will not go any deeper for the sacke of this example.
