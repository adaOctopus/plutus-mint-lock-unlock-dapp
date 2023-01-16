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