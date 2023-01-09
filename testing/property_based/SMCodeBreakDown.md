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