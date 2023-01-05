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

module Offchain.LockMintWOF where


import           Cardano.Api as Cardano
import           Cardano.Api                          (PlutusScriptV1,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope, Script)
import           Cardano.Api.Shelley                  (PlutusScript (..), PlutusScript(PlutusScriptSerialised), Address(..) )

import qualified Codec.Serialise             as Codec
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Map                             as Map
import           GHC.Generics           ( Generic )
import qualified Data.ByteString.Short                as SBS
import qualified Data.ByteString.Base16 as B16
import           Data.Functor                         (void)
import           Data.Text                   (pack, unpack, Text)
import           Data.String                                    (fromString)
import qualified Data.Maybe                           as DM (fromJust, fromMaybe)
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value
import qualified         Data.Aeson                  (decode, encode)
import           Data.Void                            (Void)
import qualified Ledger.Address                       as LAD
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as PSUV.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V1.Ledger.Scripts             as PLV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           PlutusTx                             (getPir)
import           Data.Aeson                  (decode, encode, FromJSON, ToJSON, Value (Bool))
import qualified PlutusTx
import qualified PlutusTx.Builtins
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),unless, (.))
import           Prelude                              (IO, (.), FilePath, Show, String, fromIntegral, show, div, ( <> ))
import           Prettyprinter.Extras                 (pretty)
import qualified PlutusTx.Prelude            as PPP (divide)
import qualified Ledger                      as Plutus
import           Cardano.Ledger.Credential   as Ledger
import qualified Utils                       as UTL
import qualified Ledger.Typed.Scripts        as ScriptsOne
import qualified Offchain.LockScriptWOF      as LSWOF
import           Ledger.Constraints
import qualified Ledger.Constraints   as Constraints
import Plutus.Contract as Contract 
import           Text.Printf            (printf)
import           Ledger.Ada           as Ada

--- THIS IS THE POLICY FOR THE NFT IDENTIFICATION TOKEN
--- WHEN A USER DEPOSITS FUNDS TO THE LOCKSCRIPT RECEIVES IT



nftPolicy :: PlutusV1.TxOutRef -> Plutus.Address -> () -> PlutusV1.ScriptContext -> Bool
nftPolicy txo lca _ ctx = traceIfFalse "Not enough ADA Locked" depositsEnoughAda &&
                      traceIfFalse "Wrong amount minted" checkNFT &&
                      traceIfFalse "UTXO not there" hasTheUtxo
  where
    info :: PlutusV1.TxInfo
    info = PlutusV1.scriptContextTxInfo ctx

    getTxInputs :: [PlutusV1.TxInInfo]
    getTxInputs = PlutusV1.txInfoInputs info

    getTxOutputs :: [PlutusV1.TxOut]
    getTxOutputs = PlutusV1.txInfoOutputs info

    hasTheUtxo :: Bool
    hasTheUtxo = any (\input -> PlutusV1.txInInfoOutRef input == txo ) getTxInputs

    getCurrOutputs :: [PlutusV1.TxOut]
    getCurrOutputs = PlutusV1.txInfoOutputs info

    filterLockScriptAddress :: [PlutusV1.TxOut]
    filterLockScriptAddress = P.filter (\outp -> PlutusV1.txOutAddress outp == lca ) getTxOutputs

    depositsEnoughAda :: Bool
    depositsEnoughAda = case Value.flattenValue . PlutusV1.txOutValue $ head filterLockScriptAddress of
                          [(cs, tn, amt)] -> case amt P.>= 15000000 of
                                              True -> True
                                              False -> False
                          _               -> False
    
    -- Value.flattenValue (PlutusV2.txInfoMint info)
    -- returns [(Currency, TokenName, Integer)]
    -- getOnlyTwoCryptoFields :: [(CurrencySymbol, TokenName, Integer)] -> [(CurrencySymbol, Integer)]
    -- getOnlyTwoCryptoFields [(cs, _, ing)] = [(cs, ing)]

    getOnlyTwoCryptoFields' :: [(CurrencySymbol, TokenName, Integer)] -> [(CurrencySymbol, Integer)]
    getOnlyTwoCryptoFields' = fmap $ (\(cs,_,ing) -> (cs, ing)) 
    
    -- Now we filtered for the current NFT symbol so we can check its value
    filterForCurrentCurrencySymbol :: [(CurrencySymbol, Integer)]
    filterForCurrentCurrencySymbol = P.filter ((== PlutusV1.ownCurrencySymbol ctx).fst ) (getOnlyTwoCryptoFields' . Value.flattenValue $ PlutusV1.txInfoMint info)
    
    checkNFT :: Bool
    checkNFT = case filterForCurrentCurrencySymbol of
                 [(cs, ing)] -> ing == 1
                 _           -> False
    -- checkNFTAmount :: Bool
    -- checkNFTAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
    --    [(cs, _, amt)] -> case cs  == PlutusV2.ownCurrencySymbol ctx of 
    --                        True -> amt == 1
    --                        _    -> True
    --    _                -> False


{-
    As a Minting Policy
-}

policy :: PlutusV1.TxOutRef -> Plutus.Address -> Scripts.MintingPolicy
policy txo adr = PlutusV1.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode txo
     `PlutusTx.applyCode`
     PlutusTx.liftCode adr
  where
    wrap txo' adr' = PSU.V1.mkUntypedMintingPolicy $ nftPolicy txo' adr'

{-
    As a Script
-}

curSymbol :: PlutusV1.TxOutRef -> Plutus.Address -> CurrencySymbol
curSymbol oref ad = Plutus.scriptCurrencySymbol $ policy oref ad

script :: PlutusV1.TxOutRef -> Plutus.Address -> PlutusV1.Script
script txo adr = PlutusV1.unMintingPolicyScript $ (policy txo adr)

{-
    As a Short Byte String
-}

scriptSBS :: PlutusV1.TxOutRef -> Plutus.Address -> SBS.ShortByteString
scriptSBS txo adr = SBS.toShort . LBS.toStrict . serialise $ (script txo adr)

{-
    As a Serialised Script
-}

serialisedScript :: PlutusV1.TxOutRef -> Plutus.Address -> PlutusScript PlutusScriptV2
serialisedScript txo adr = PlutusScriptSerialised $ (scriptSBS txo adr)

writeSerialisedScript :: PlutusV1.TxOutRef -> Plutus.Address -> IO ()
writeSerialisedScript txo adr = void $ writeFileTextEnvelope "scripts/nft-mint-V2.plutus" Nothing (serialisedScript txo adr)


------------------
-- Offchain Code
------------------

data ScenarioParams = ScenarioParams
    { npToken   :: !TokenName
    , npAddress :: !Plutus.Address
    } deriving (Generic, FromJSON, ToJSON, Show)

PlutusTx.makeLift ''ScenarioParams
PlutusTx.unstableMakeIsData ''ScenarioParams

type ScenarioSchema = Endpoint "lockMintUnlock" ScenarioParams

testingRedeemerX :: LSWOF.UserAction
testingRedeemerX = LSWOF.Unlock 100000000 42

scenarioSimulation :: ScenarioParams -> LSWOF.LockParams -> Contract () ScenarioSchema Text ()
scenarioSimulation sp lp = do
    Contract.logInfo @String $ "1: pay to the script address"
    let dat = LSWOF.LockDatum
                { LSWOF.depositAmount = LSWOF.adaMount lp
                , LSWOF.ownerKeyHash    = LSWOF.userAddr lp
                }
        -- mustPayToOtherScript expects datum of type Ledger.Datum, so we need to convert whatever custom datum we have
        datumFormatter = (Plutus.Datum $ PlutusTx.toBuiltinData dat)
    let tx1 = Constraints.mustPayToOtherScript LSWOF.valHash datumFormatter $ Ada.lovelaceValueOf (LSWOF.adaMount lp)
    ledgerTx1 <- submitTx tx1
    void $ awaitTxConfirmed $ Plutus.getCardanoTxId ledgerTx1
    logInfo @String $ "tx1 successfully submitted"

    Contract.logInfo @String $ "2: mint the tokens"
    utxos <- utxosAt $ npAddress sp
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken sp
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx2     = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
            void $ awaitTxConfirmed $ Plutus.getCardanoTxId ledgerTx2
            Contract.logInfo @String $ printf "forged %s" (show val)
    let sParams = (Plutus.Datum $ PlutusTx.toBuiltinData sp)
    logInfo @String $ "tx2 successfully submitted"

    Contract.logInfo @String $ "3: unlock the funds from script address"
    scriptUtxos <- utxosAt LSWOF.scrAddress
    let scriptOrefs = fst <$> Map.toList scriptUtxos
        lookupScript =
            Constraints.plutusV1OtherScript LSWOF.validatorV1
            <> Constraints.unspentOutputs scriptUtxos
        tx3 =
            mconcat [Constraints.mustSpendScriptOutput oref (Plutus.Redeemer $ PlutusTx.toBuiltinData testingRedeemerX) | oref <- scriptOrefs]
            <> Constraints.mustIncludeDatum datumFormatter -- List comprehension -- Changing redeemer value correctly throws ValidationError
            -- <> Constraints.mustValidateIn (to $ 1596059100000) -- doesn't seem to care what datum is
    ledgerTx3 <- submitTxConstraintsWith @Void lookupScript tx3
    logInfo @String $ "waiting for tx3 confirmed..."
    awaitTxConfirmed $ Plutus.getCardanoTxId ledgerTx3
    logInfo @String $ "tx3 successfully submitted"