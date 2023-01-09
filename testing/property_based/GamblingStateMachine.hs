{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Make sure you import all of the above
-- | Simply copy pasting what is in plutus-apps/ use cases for GameStateMachine, does not work. I had to add 5 new more extensions.


module GamblingStateMachine where


import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C
import GHC.Generics (Generic)
import Ledger (Address, POSIXTime, ScriptContext, TokenName, Value)
import Ledger.Ada qualified as Ada
import Ledger.Address.Orphans ()
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as V
import Plutus.Contract (AsContractError (_ContractError), Contract, ContractError, Endpoint, Promise, endpoint,
                        selectList, type (.\/))
import Plutus.Contract.Secrets (SecretArgument, escape_sha2_256, extractSecret)
import Plutus.Contract.StateMachine (State (State, stateData, stateValue), Void)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False, True), BuiltinByteString, Eq, Maybe (Just, Nothing), check, sha2_256, toBuiltin,
                         traceIfFalse, ($), (&&), (-), (.), (<$>), (<>), (==), (>>))
import Schema (ToSchema)
import Plutus.Contract.Test.Coverage.Analysis
import PlutusTx.Coverage
import Prelude qualified as Haskell


-- | This StateMachine is similar to the IOHK GuessingGameStateMachine, but a bit more simplified.
-- | The secret password does not get updated when someone is winning a bet. The betting simply ends.
-- | It is simplified because it is meant just for practice and as an introduction to StateMachines. It is the first time I implement one.


-- | This is the datatype for the parameters of the validator/transition

data GambleParam = GambleParam {

    initiatorAddr :: Address

} deriving (Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''GambleParam


-- | Secret password to attach to the state for the gamblers

newtype HashedString = HashedString BuiltinByteString
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ClearString



-- | Parameters for when a user locks the initial bet
-- | Arguments for the @"lockbet"@ endpoint

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



-- | The schema of the contract. It consists of the two endpoints @"lockbet"@
--   and @"makebet"@ with their respective argument types.
type GambleStateMachineSchema =
        Endpoint "lockbet" LockBetArgs
        .\/ Endpoint "makebet" MakeBetArgs


-- | Instance of capturing errors

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


-- | Top-level contract, exposing both endpoints.
contract :: Contract () GambleStateMachineSchema GambleError ()
contract = selectList [lockie, bettie] >> contract
---------------------- ^ lockie & bettie, are the endpoints for this state machine

-- | The token that represents the right to make a bet
newtype GambleToken = GambleToken { unGambleToken :: Value }
    deriving newtype (Eq, Haskell.Show)

token :: MintingPolicyHash -> TokenName -> Value
token mps tn = V.singleton (V.mpsSymbol mps) tn 1




-- | The State of this State Machine

data GambleState = 
    Started MintingPolicyHash TokenName HashedString
    -- ^ initialized the gambling state machine

    | LockedBet MintingPolicyHash TokenName HashedString
    -- ^ funds are locked, only betting can occur now.

    | EndBet 
    -- ^ Gamble ends, redistribution of funds.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


instance Eq GambleState where
    {-# INLINABLE (==) #-}
    (Started sym tn s) == (Started sym' tn' s')               = sym == sym' && s == s' && tn == tn'
    (LockedBet sym tn s) == (LockedBet sym' tn' s')           = sym == sym' && s == s' && tn == tn'
    EndBet == EndBet                                          = True
    _ == _                                                    = traceIfFalse "states not equal" False


-- | Check whether a 'ClearString' is matching the hash
checkBetPass :: HashedString -> ClearString -> Bool
checkBetPass (HashedString actual) (ClearString gss) = actual == sha2_256 gss


-- | Inputs (actions) BAsically what can someone do when interacting with this StateMachine.
data GambleInput =
      MintToken
    -- ^ Mint the "bet" token
    | Bet Address ClearString HashedString Value
    -- ^ Make a bet, extract the funds if you win.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)