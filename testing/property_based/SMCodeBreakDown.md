### State Machine Code Breakdown

This document's purpose is to explain on a top level the code you need in order to implement a [StateMachine] in Plutus.

1. First we need our parameter data type for the validator and the transition function. In our case it is just a record type with one field

```

data GambleParam = GambleParam {

    initiatorAddr :: Address

} deriving (Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''GambleParam

```

2. We also need parameters for each endpoint we will instantiate. IN our case is to lock the funds to the gambling machine, and one to make a bet.

```
data LockBetArgs =
    LockBetArgs
        { lockArgsGambleParam     :: GambleParam
        -- ^ The parameters for parameterizing the validator.
        , lockArgsGambleSecret    :: SecretArgument Haskell.String
        -- ^ The secret
        , lockArgsGAmbleValue     :: Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)


-- | Parameters for when a user makes a bet
-- | This is simpler than the GameStateMachine from IOHK, because we dont update password here. if you win it is done.
-- | However, for the next betting we need to check if there are funds inside the initial state.

data MakeBetArgs =
    MakeBetArgs
        {
            makeBetGambleArgs   :: GambleParam,
            passTokenGambleArgs :: Address,
            actualBetGambleArgs :: Value
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

```

3. We also make custom newtypes for the secret password that will be added to the state. `Lines 79-91` view GamblingStateMachine.hs

4. `Line 127` we are making our endpoint schema . view GamblingStateMachine.hs

5. We also need to create an instance for error types, in order to capture potential errors in the contract instance and the state machine
   also, enabling lenses & optics

```
data GambleError =
    GambleContractError ContractError
    | GambleSMError SM.SMContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- this enables using lens & optics
makeClassyPrisms ''GambleError

instance AsContractError GambleError where
    _ContractError = _GambleContractError . _ContractError

instance SM.AsSMContractError GambleError where
    _SMContractError = _GambleSMError . SM._SMContractError

```


6. Lines `167-185` we define the State of the state machine, Started, Locked, EndBet

7. Top level contract instance and the type for the actual token needed for someone to make a bet.

```
-- | Top-level contract, exposing both endpoints.
contract :: Contract () GambleStateMachineSchema GambleError ()
contract = selectList [lockie, bettie] >> contract
---------------------- ^ lockie & bettie, are the endpoints for this state machine

-- | The token that represents the right to make a bet
newtype GambleToken = GambleToken { unGambleToken :: Value }
    deriving newtype (Eq, Haskell.Show)

token :: MintingPolicyHash -> TokenName -> Value
token mps tn = V.singleton (V.mpsSymbol mps) tn 1

```

8. Possible actions for someone to interact with the contract itself.

```


-- | Inputs (actions) BAsically what can someone do when interacting with this StateMachine.
data GambleInput =
      MintToken
    -- ^ Mint the "bet" token
    | Bet Address ClearString HashedString Value
    -- ^ Make a bet, extract the funds if you win.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

```

9. From line `239-272` is pretty much boilerplate code for the
  machineClient, which is a way to interact with the statemachine from offchain code
  typedvalidators, minting policy etc