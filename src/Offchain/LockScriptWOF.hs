{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE RankNTypes            #-}
-- {-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wall #-}

module Offchain.LockScriptWOF where


import           Cardano.Api as Cardano
import           Cardano.Api                          (PlutusScriptV1,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope, Script)
import           Cardano.Api.Shelley                  (PlutusScript (..), PlutusScript(PlutusScriptSerialised), Address(..) )

import qualified Codec.Serialise             as Codec
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS

import           GHC.Generics           ( Generic )
import qualified Data.ByteString.Short                as SBS
import qualified Data.ByteString.Base16 as B16
import           Data.Functor                         (void)
import           Data.Void                            (Void)
import           Data.Map                             as Map
import           Data.Text                   (pack, unpack, Text)
import           Data.String                                    (fromString)
import qualified Data.Maybe                           as DM (fromJust, fromMaybe)
import           Ledger.Value                         as Value
import qualified         Data.Aeson                  (decode, encode)
import qualified Ledger.Address                       as LAD
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as PSUV.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V1.Ledger.Scripts             as PLV1
import qualified Ledger.Typed.Scripts                 as ScriptsOne
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified Ledger.Typed.Scripts as Scripts
import           PlutusTx                             (getPir)
import           Data.Aeson                  (decode, encode, FromJSON, ToJSON)
import qualified PlutusTx
import qualified PlutusTx.Builtins
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),unless)
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import Plutus.Contract as Contract 
import Ledger 
import           Prelude              (IO, Semigroup (..), Show (..), String)
import qualified Prelude
import           Ledger.Ada           as Ada
import           Schema               (ToSchema)
import           Text.Printf            (printf)
import qualified Ledger as Scripts
import qualified Ledger as PSU.V2
import qualified Ledger.Typed.Scripts as PLV1
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
-- What does it do?
-- Stores info regarding how much ALice deposited
-- 2 Actions, Lock & UNlock the funds


data LockDatum = LockDatum {

    depositAmount :: !Integer,
    ownerKeyHash  :: PubKeyHash

} deriving (Show, FromJSON, ToJSON, Generic)

instance Eq LockDatum where
    {-# INLINABLE (==) #-}
    LockDatum d1 p1 == LockDatum d2 p2 = p1 == p2 && d1 == d2

PlutusTx.makeLift ''LockDatum
PlutusTx.unstableMakeIsData ''LockDatum

type AmountToUnlock = Integer
type Password       = Integer

data UserAction = Unlock AmountToUnlock Password deriving (Show, FromJSON, ToJSON, Generic)

PlutusTx.makeLift ''UserAction
PlutusTx.makeIsDataIndexed ''UserAction
 [
   ( 'Unlock, 1 )
 ]

{-# INLINEABLE lockScript #-}
lockScript :: LockDatum -> UserAction -> PlutusV1.ScriptContext -> Bool
lockScript dt rd ctx = traceIfFalse "Oops wrong password" checkPass &&
                       traceIfFalse "Oops not correct signature" checkSign
    
    where

      info :: PlutusV1.TxInfo
      info =  PlutusV1.scriptContextTxInfo ctx

      checkSign :: Bool
      checkSign = PlutusV1.txSignedBy info $ ownerKeyHash dt

      checkPass :: Bool
      checkPass = case rd of 
                    Unlock amt psw -> psw == (42 :: Integer)
                    _              -> False

      checkAmou :: Bool
      checkAmou = case rd of
                   Unlock amt _ -> amt == depositAmount dt
                   _            -> False
      
      getTxInputs :: [PlutusV1.TxInInfo]
      getTxInputs = PlutusV1.txInfoInputs info
      
      depositsEnoughAda :: Bool
      depositsEnoughAda = case Value.flattenValue . PlutusV1.txOutValue . PlutusV1.txInInfoResolved $ head getTxInputs of
                            [(cs, tn, amt)] -> case amt P.>= 10000000 of
                                                True -> True
                                                False -> False
                            _               -> False


-- untypedValidatorV2 :: PSU.V2.Validator -- There is not yet a way to make a V2 typed validator (PLT-494)
-- untypedValidatorV2 = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = PSU.V2.mkUntypedValidator lockScript

data Typed
instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = LockDatum
  type RedeemerType Typed = UserAction

typedValidatorX :: ScriptsOne.TypedValidator Typed
typedValidatorX =
    Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [||lockScript||])
    $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V1.mkUntypedValidator


validatorV1 :: PlutusV1.Validator
validatorV1 = PLV1.validatorScript typedValidatorX

valHash :: PSU.V2.ValidatorHash
valHash = ScriptsOne.validatorHash typedValidatorX

scrAddress :: Ledger.Address
scrAddress = ScriptsOne.validatorAddress typedValidatorX

lockingScript :: PlutusV1.Script
lockingScript = PlutusV1.unValidatorScript validatorV1


{-
    As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict . serialise $ lockingScript

{-
    As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised $ scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "locking-v2.plutus" Nothing serialisedScript


-- OFFCHAIN PART STARTS

data LockParams = LockParams {
    userAddr :: PubKeyHash,
    adaMount :: !Integer
} deriving (Generic, ToJSON, FromJSON, ToSchema)

-- newtype UnlockParams = UnlockParams {
--     infoUser :: UserAction
-- }  deriving (Generic, ToJSON, FromJSON, ToSchema)


type LockingSchema = Endpoint "lock" LockParams .\/ Endpoint "unlock" LockParams


-- V2 DOES NOT WORK FOR TYPED VALIDATORS
-- typedValidator :: PSU.V2.Validator 
-- typedValidator = PLV1.mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
--    where
--       wrap = PSUV.V2.mkUntypedValidator lockScript

testingRedeemer :: UserAction
testingRedeemer = Unlock 100000000 467

contractLock :: LockParams -> Contract () LockingSchema Text ()
contractLock lp = do
    now <- currentTime
    Contract.logInfo @String $ "now: " ++ show now
    Contract.logInfo @String $ "1: pay to the script address"
    let dat = LockDatum
                { depositAmount = adaMount lp
                , ownerKeyHash    = userAddr lp
                }
        -- mustPayToOtherScript expects datum of type Ledger.Datum, so we need to convert whatever custom datum we have
        datumFormatter = (Scripts.Datum $ PlutusTx.toBuiltinData dat)
    let tx1 = Constraints.mustPayToOtherScript valHash datumFormatter $ Ada.lovelaceValueOf (adaMount lp)
    ledgerTx1 <- submitTx tx1
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx1
    logInfo @String $ "tx1 successfully submitted"

    logInfo @String $ "2: spend from script address including datum and redeemer 'CUSTOM ;)'"
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups =
            Constraints.plutusV1OtherScript validatorV1
            <> Constraints.unspentOutputs utxos
        tx2 =
            mconcat [Constraints.mustSpendScriptOutput oref (Scripts.Redeemer $ PlutusTx.toBuiltinData testingRedeemer) | oref <- orefs]
            <> Constraints.mustIncludeDatum datumFormatter -- List comprehension -- Changing redeemer value correctly throws ValidationError
            -- <> Constraints.mustValidateIn (to $ 1596059100000) -- doesn't seem to care what datum is
    ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
    logInfo @String $ "waiting for tx2 confirmed..."
    awaitTxConfirmed $ getCardanoTxId ledgerTx2
    logInfo @String $ "tx2 successfully submitted"


endpoints :: Contract () LockingSchema Text ()
endpoints = awaitPromise (lock' `select` unlock') >> endpoints
  where
    lock' = endpoint @"lock" contractLock
    unlock' = endpoint @"unlock" contractLock

mkSchemaDefinitions ''LockingSchema