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
{-# LANGUAGE NamedFieldPuns  #-}
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
import qualified Ledger.Address                       as LAD
import qualified Utils as UTL
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
                         traceIfFalse, ($), (&&), (-), (.), (+), (<$>), (<>), (==), (>>))
import Schema (ToSchema)
import Plutus.Contract.Test.Coverage.Analysis
import PlutusTx.Coverage
import Prelude qualified as Haskell


-- | This StateMachine is similar to the IOHK GuessingGameStateMachine, but a bit more simplified.
-- | The secret password does not get updated when someone is winning a bet. The betting simply ends.
-- | It is simplified because it is meant just for practice and as an introduction to StateMachines. It is the first time I implement one.


-- | This is the datatype for the parameters of the validator/transition

data GambleParam = GambleParam {

    initiatorAddr :: LAD.PaymentPubKeyHash

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
        , lockArgsGambleValue     :: Value
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
            passTokenGambleArgs :: LAD.PaymentPubKeyHash,
            guessedPassGambleArgs :: Haskell.String,
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
contract = selectList [lockBet, makeBet] >> contract
---------------------- ^ are the endpoints for this state machine

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
    | Bet LAD.PaymentPubKeyHash ClearString Value
    -- ^ Make a bet, extract the funds if you win.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


-- | The State Machine Transition Function

{-# INLINABLE transition#-}
transition :: GambleParam -> State GambleState -> GambleInput -> Maybe (TxConstraints Void Void, State GambleState)
transition _ State{stateData=stateZeroData, stateValue=stateZeroValue} gambleInput = case (stateZeroData, gambleInput) of
    ( Started mph tn hs, MintToken ) -> 
        let constraints = Constraints.mustMintCurrency mph tn 1 in
        Just ( constraints,
               State{
                stateData  = LockedBet mph tn hs,
                stateValue = stateZeroValue
               }) -- LAD.unPaymentPubKeyHash $ UTL.unsafePaymentPubKeyHash 
    ( LockedBet mph tn currentHs, Bet tokenRecipient cs betValue) -> case (checkBetPass currentHs cs) of
                                                                             True -> let constraints = Constraints.mustPayToPubKey tokenRecipient (token mph tn)
                                                                                                       <> Constraints.mustMintCurrency mph tn 0
                                                                                         newValue    = stateZeroValue - betValue
                                                                                     in Just (constraints , 
                                                                                              State { 
                                                                                                stateData = if V.isZero (Ada.toValue $ Ada.fromValue newValue) 
                                                                                                               then EndBet
                                                                                                               else LockedBet mph tn currentHs ,
                                                                                                stateValue = newValue
                                                                                                               })
                                                                             False -> let constraints = Constraints.mustPayToPubKey tokenRecipient (token mph tn)
                                                                                                       <> Constraints.mustMintCurrency mph tn 0
                                                                                      in Just ( constraints, 
                                                                                                State {
                                                                                                    stateData = LockedBet mph tn currentHs,
                                                                                                    stateValue = stateZeroValue + betValue
                                                                                                })
         
    _                                -> Nothing


-- | Boilerplate for validator & TH for compiling the code
type GambleStateMachine = SM.StateMachine GambleState GambleInput

{-# INLINABLE machine #-}
machine :: GambleParam -> GambleStateMachine
machine gambleParam = SM.mkStateMachine Nothing (transition gambleParam) isFinal where
    isFinal EndBet = True
    isFinal _      = False

{-# INLINABLE mkValidator #-}
mkValidator :: GambleParam -> Scripts.ValidatorType GambleStateMachine
mkValidator gambleParam = SM.mkValidator (machine gambleParam)


typedValidator :: GambleParam -> Scripts.TypedValidator GambleStateMachine
typedValidator = Scripts.mkTypedValidatorParam @GambleStateMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

mintingPolicy :: GambleParam -> Scripts.MintingPolicy
mintingPolicy gp = Scripts.forwardingMintingPolicy $ typedValidator gp

client :: GambleParam -> SM.StateMachineClient GambleState GambleInput
client gp = SM.mkStateMachineClient $ SM.StateMachineInstance (machine gp) $ typedValidator gp


-- | Actual endpoint functionality

-- | The @"lockbet"@ endpoint.
lockBet :: Promise () GambleStateMachineSchema GambleError ()
lockBet = endpoint @"lockbet" $ \LockBetArgs{lockArgsGambleParam, lockArgsGambleSecret, lockArgsGambleValue} -> do
    let secret = HashedString (escape_sha2_256 (toBuiltin . C.pack <$> extractSecret lockArgsGambleSecret))
        sym = Scripts.forwardingMintingPolicyHash $ typedValidator lockArgsGambleParam
    _ <- SM.runInitialise (client lockArgsGambleParam) (Started sym "bettoken" secret) lockArgsGambleValue
    void $ SM.runStep (client lockArgsGambleParam) MintToken


-- | The @"makebet"@ endpoint

-- | The @"guess"@ endpoint.
makeBet :: Promise () GambleStateMachineSchema GambleError ()
makeBet = endpoint @"makebet" $ \MakeBetArgs{makeBetGambleArgs, passTokenGambleArgs, guessedPassGambleArgs, actualBetGambleArgs} -> do

    let guessedSecret = ClearString (toBuiltin (C.pack guessedPassGambleArgs))
        -- newSecret     = HashedString (escape_sha2_256 (toBuiltin . C.pack <$> extractSecret (guessedPassGambleArgs)))

    void
        $ SM.runStep (client makeBetGambleArgs)
            (Bet passTokenGambleArgs guessedSecret actualBetGambleArgs)


-- | Turn in to PlutusCore BOILERPLATE
cc :: PlutusTx.CompiledCode (GambleParam -> GambleState -> GambleInput -> ScriptContext -> ())
cc = $$(PlutusTx.compile [|| \a b c d -> check (mkValidator a b c d) ||])

PlutusTx.unstableMakeIsData ''GambleState
PlutusTx.makeLift ''GambleState
PlutusTx.unstableMakeIsData ''GambleInput
PlutusTx.makeLift ''GambleInput
PlutusTx.makeLift ''GambleToken
PlutusTx.unstableMakeIsData ''GambleToken