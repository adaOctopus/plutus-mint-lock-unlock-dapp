### State Machine Code Breakdown

This document's purpose is to explain on a top level the code you need in order to implement a [StateMachine] in Plutus.

1. First we need our parameter data type for the validator and the transition function. In our case it is just a record type with one field

```

data GambleParam = GambleParam {

    initiatorAddr :: Address

} deriving (Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''GambleParam

```