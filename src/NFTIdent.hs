{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module NFTIdent where


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
import           Data.Text                   (pack, unpack)
import           Data.String                                    (fromString)
import qualified Data.Maybe                           as DM (fromJust, fromMaybe)
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value
import qualified         Data.Aeson                  (decode, encode)
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
import           Data.Aeson                  (decode, encode, FromJSON, ToJSON)
import qualified PlutusTx
import qualified PlutusTx.Builtins
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),unless, (.))
import           Prelude                              (IO, (.), FilePath, Show, String, fromIntegral, show, div)
import           Prettyprinter.Extras                 (pretty)
import qualified PlutusTx.Prelude            as PPP (divide)
import qualified Ledger                      as Plutus
import           Cardano.Ledger.Credential   as Ledger

--- THIS IS THE POLICY FOR THE NFT IDENTIFICATION TOKEN
--- WHEN A USER DEPOSITS FUNDS TO THE LOCKSCRIPT RECEIVES IT


nftPolicy :: PlutusV2.TxOutRef -> () -> PlutusV2.ScriptContext -> Bool
nftPolicy txo _ ctx = traceIfFalse "Not enough ADA Locked" depositsEnoughAda &&
                      traceIfFalse "Wrong amount minted" checkNFTAmount
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    getTxInputs :: [PlutusV2.TxInInfo]
    getTxInputs = PlutusV2.txInfoInputs info

    hasTheUtxo :: Bool
    hasTheUtxo = any (\input -> PlutusV2.txInInfoOutRef input == txo ) getTxInputs

    getCurrOutputs :: [PlutusV2.TxOut]
    getCurrOutputs = PlutusV2.txInfoOutputs info

    depositsEnoughAda :: Bool
    depositsEnoughAda = case Value.flattenValue . PlutusV2.txOutValue . PlutusV2.txInInfoResolved $ head getTxInputs of
                          [(cs, tn, amt)] -> case amt P.>= 15000000 of
                                              True -> True
                                              False -> False
                          _               -> False
    
    checkNFTAmount :: Bool
    checkNFTAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
       [(cs, _, amt)] -> cs  == PlutusV2.ownCurrencySymbol ctx && amt == 1
       _                -> False


{-
    As a Minting Policy
-}

policy :: PlutusV2.TxOutRef -> Scripts.MintingPolicy
policy txo = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode txo
  where
    wrap txo' = PSU.V2.mkUntypedMintingPolicy $ nftPolicy txo'

{-
    As a Script
-}

script :: PlutusV2.TxOutRef -> PlutusV2.Script
script txo = PlutusV2.unMintingPolicyScript $ policy txo

{-
    As a Short Byte String
-}

scriptSBS :: PlutusV2.TxOutRef -> SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict . serialise . script

{-
    As a Serialised Script
-}

serialisedScript :: PlutusV2.TxOutRef -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . scriptSBS

writeSerialisedScript :: PlutusV2.TxOutRef -> IO ()
writeSerialisedScript txo = void $ writeFileTextEnvelope "nft-mint-V2.plutus" Nothing (serialisedScript txo)