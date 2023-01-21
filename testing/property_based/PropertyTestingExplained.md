### Property Based Testing Explained

Codefile: `TestGamblingSM.hs`


#### Components

imports in repl, import TestGamblingSM Test.QuickCheck Plutus.Contract.Test.ContractModel

This is my first time trying out property based testing, and I must admit, is not the easiest thing I have ever done :)

There are a lot of moving components required to make it work.

Simply put.

When we do property based testing, we running a simulation of our SYstem (COntract  functionality) and a Model of what we expect to achieve, and at the end
we compare those two.

1. We need to create a parameter for our Model, which needs to match the Param we include in the transition function of our state machine. In this case, wallet address

```
gambleParam :: G.GambleParam
gambleParam = G.GambleParam (UTL.unsafePaymentPubKeyHash $ mockWalletAddress w1)

```

2. We need to define a data type for our model. THis does not have to much fully the datum/state data type of our state machine contract
However, it needs to be able to answer the questions of our logic check.

```
data GambleModel = GambleModel
    { _gambleValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String
    }
    deriving (Show, Data, Generic)

makeLenses 'GambleModel


deriving instance Eq (ContractInstanceKey GambleModel w schema err params)
deriving instance Ord (ContractInstanceKey GambleModel w schema err params)
deriving instance Show (ContractInstanceKey GambleModel w schema err params)
deriving instance Arbitrary GambleModel
```

3. IN order to begin we need to define an instance of our model and the actions that can be done.


```
instance ContractModel GambleModel where
    
    -- | Every contract instance is identified by ContractInstanceKey
    -- | That way quickCheck knows what code to run, the schema of the endpoints and the error types available
    data ContractInstanceKey GambleModel w schema err params where
        WalletKey :: Wallet -> ContractInstanceKey GambleModel () GambleStateMachineSchema GambleError ()

    -- The commands available to a test case
    data Action GambleModel = Lock      Wallet String Integer
                            | BetA     Wallet String Integer
                            | GiveToken Wallet
        deriving (Eq, Show, Data, Arbitrary)

```

4. Assuming you are familiar with quickCHeck and how it works with the Gen monad and randomly generated data, here it is a similar concept.

A generated test is called Actions, and is, as the name suggests, essentially a sequence of Action values. We can run tests by using propRunActions_:

```
prop_Gamble :: Actions GambleModel -> Property
prop_Gamble actions = propRunActions_ actions
```

When we test this property, quickCheck will generate random action sequences to be tested, checking at the end of each test that tokens are transferred correctly, and contracts didn’t crash

5. But how does quickCheck know what code to run when you check prop_Gamble? 

propRunActions_ needs to create a handle for each contract instance, which is used to invoke their endpoints from the test. Different contracts have different endpoints, of different types–and thus different schemas. 

TO achieve this we use COntractInstanceKey instance

Every contract instance in a test is named by a ContractInstanceKey, another associated datatype of the ContractModel class; we talk to a contract instance by referring to its ContractInstanceKey

```
   data ContractInstanceKey GambleModel w schema err params where
        WalletKey :: Wallet -> ContractInstanceKey GambleModel () GambleStateMachineSchema GambleError ()

```

6. Once this type is defined, we can tell QuickCheck what code to run for a given contract by filling in the initialInstances, instanceWallet, and instanceContract fields of the ContractModel class:

    initialInstances = (`StartContract` ()) . WalletKey <$> wallets

    instanceContract _ WalletKey{} _ = G.contract

    instanceWallet (WalletKey w) = w
This specifies (reading top to bottom) that we should create one contract instance per wallet w, that will run G.contract, in wallet w.

7. In order to make sure that there is an arbitrary instance of our Model so we can generate random actions we need to include this function 

```
    arbitraryAction :: ModelState GambleModel -> Gen (Action GambleModel)
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue ] ++
        [ frequency $
          [ (10, BetA w   <$> genGuess  <*> choose (0, val))
          | Just w <- [tok] ] ++
          [ (1, BetA <$> genWallet <*> genGuess <*> genValue) ] ] ++
        [ GiveToken <$> genWallet ]
        where
            tok = s ^. contractState . hasToken
            val = s ^. contractState . gambleValue

-- And we need these to be able to generate random ones as parameters

genWallet :: Gen Wallet
genWallet = elements wallets

genGuess :: Gen String
genGuess = elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary

```

8. NOw we need to define the initial state of the contract when the token does not exist, and functions to move from one state to the other (initialSTate, and nextState)


```
    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }

            -- | First state after the initial state, locks funds and mints
    nextState (Lock w secret val) = do
        hasToken      .= Just w
        currentSecret .= secret
        gambleValue   .= val
        mint betTokenVal
        deposit w betTokenVal
        withdraw w $ Ada.lovelaceValueOf val
        wait 2
    
    -- | Next possible state of Betting action
    nextState (BetA w old val) = do
        correctGuess <- (old ==)    <$> viewContractState currentSecret
        holdsToken   <- (Just w ==) <$> viewContractState hasToken
        enoughAda    <- (val <=)    <$> viewContractState gambleValue
        when (correctGuess && holdsToken && enoughAda) $ do
            currentSecret $= old
            gambleValue     $~ subtract val
            deposit w $ Ada.lovelaceValueOf val
        wait 1

    -- | State when passing the betting token
    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewContractState hasToken
        transfer w0 w betTokenVal
        hasToken $= Just w

```

9. As we just saw, not every sequence of actions makes sense as a test case; we need a way to restrict test cases to be ‘sensible’. 

To introduce preconditions, we add a definition of the precondition method to our ContractModel instance.

```
precondition :: ModelState state -> Action state -> Bool
```
The precondition is parameterised on the entire model state, which includes the contents of wallets as well as our contract state, so we will need to extract this state as well as the fields we need from it. For now, we just restrict GiveToken actions to states in which the token exists:

```
    precondition s (GiveToken _) = isJust tok
        where
            tok = s ^. contractState . hasToken
    precondition s _             = True
```

10. Executing and testing

i.e in the repl we would run quickCheck prop_Gamble

```
prop_Gamble :: Actions GambleModel -> Property
prop_Gamble = propRunActions_

-- | Property wiith a more well designed LOgMessage
propGamble' :: LogLevel -> Actions GambleModel -> Property
propGamble' l s = propRunActionsWithOptions
                    (set minLogLevel l defaultCheckOptionsContractModel)
                    defaultCoverageOptions
                    (\ _ -> pure True)
                    s

testLock :: Property
testLock = withMaxSuccess 1 . prop_Gamble $ actionsFromList [Lock w1 "hunter2" 0]

```

11. When testing make sure u can generate actions of your model with

``` sample (arbitrary :: Gen (Actions GambleModel)) ```

and make sure quickCheck works with ``` quickCheck prop_Gamble```
This was a brief explanation of the testing method. THere is way more to it, but time is finite.
Enjoy...