{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module TestGamblingSM where


-- | These two imports require a latest git tag from plutus apps where emulator is available
-- import Cardano.Node.Emulator.Params qualified as Params
-- import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import Data.Data
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Prettyprinter
import Test.QuickCheck as QC hiding (checkCoverage, (.&&.))
import Test.Tasty hiding (after)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Plutus.Contract.Secrets
import Plutus.Contract.Test hiding (not)
-- | This import is for unit testing, I still dont understand it, so I dont use it.
-- import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import GamblingStateMachine as G
import Plutus.Trace.Emulator as Trace
import Wallet.Emulator.Wallet
import PlutusTx.Coverage
import qualified Utils as UTL


-- | Parameters of gamble contract
gambleParam :: G.GambleParam
gambleParam = G.GambleParam (UTL.unsafePaymentPubKeyHash $ mockWalletAddress w1)

options :: CheckOptions
options = defaultCheckOptionsContractModel & (increaseTransactionLimits . increaseTransactionLimits)

--
-- * QuickCheck model

data GambleModel = GambleModel
    { _gambleValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String
    }
    deriving (Show, Data)

makeLenses 'GambleModel


deriving instance Eq (ContractInstanceKey GambleModel w schema err params)
deriving instance Ord (ContractInstanceKey GambleModel w schema err params)
deriving instance Show (ContractInstanceKey GambleModel w schema err params)
deriving instance HasActions GambleModel


-- | Our needed data and helper functions

wallets :: [Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen Wallet
genWallet = QC.elements wallets

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = QC.elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary
-- Here, minAdaTxOutEstimated, is not available in my current build, proparly new stuff aded by plutus team
-- genValue = choose (Ada.getLovelace Ledger.minAdaTxOutEstimated, 100_000_000)


-- | Define the ContractModel

instance ContractModel GambleModel where
    
    -- | Every contract instance is identified by ContractInstanceKey
    -- | That way quickCheck knows what code to run, the schema of the endpoints and the error types available
    data ContractInstanceKey GambleModel w schema err params where
        WalletKey :: Wallet -> ContractInstanceKey GambleModel () GambleStateMachineSchema GambleError ()

    -- The commands available to a test case
    data Action GambleModel = Lock      Wallet String Integer
                            | BetA     Wallet String Integer
                            | GiveToken Wallet
        deriving (Eq, Show, Generic, Data)
    
    initialState = GambleModel
        { _gambleValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }

    initialInstances = (`StartContract` ()) . WalletKey <$> wallets
    instanceWallet (WalletKey w) = w
    instanceContract _ WalletKey{} _ = G.contract

    -- | Arbitrary actions to test based on available endpoints
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue              ] ++
        [ BetA     <$> genWallet <*> genGuess <*> genValue ] ++
        [ GiveToken <$> genWallet                                        ]


    -- | Perform is how the generated actions are linked to our actual contract under testing, in our case GambleStateMachine
    perform handle _ s cmd = case cmd of
        Lock w new val -> do
            callEndpoint @"lockbet" (handle $ WalletKey w)
                LockBetArgs { lockArgsGambleParam = gambleParam
                         , lockArgsGambleSecret   = secretArg new
                         , lockArgsGambleValue    = Ada.lovelaceValueOf val
                         }
            delay 2
        BetA w old val -> do
            callEndpoint @"makebet" (handle $ WalletKey w)
                MakeBetArgs{ makeBetGambleArgs     = gambleParam
                         , guessedPassGambleArgs     = old
                         , actualBetGambleArgs = Ada.lovelaceValueOf val }
            delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. contractState . hasToken)
            payToWallet w w' betTokenVal
            delay 1



    -- | First state after the initial state, locks funds and mints
    nextState (Lock w secret val) = do
        hasToken      .= Just w
        currentSecret .= secret
        gambleValue     .= val
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

-- | This is required for getALlSYmTokens
instance CrashTolerance GambleModel where
  available (Lock w _ _) alive    = (Key $ WalletKey w) `elem` alive
  available (BetA w _ _) alive = (Key $ WalletKey w) `elem` alive
  available _ _                   = True

  restartArguments _ WalletKey{} = ()

betTokenVal :: Value
betTokenVal =
    let sym = Scripts.forwardingMintingPolicyHash $ G.typedValidator gambleParam
    in G.token sym "bet"

prop_Game :: Actions GambleModel -> Property
prop_Game = propRunActions_