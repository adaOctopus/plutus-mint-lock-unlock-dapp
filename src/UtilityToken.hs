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

module UtilityToken where


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
import qualified Plutus.V1.Ledger.Ada                 as ADAV1
import           PlutusTx                             (getPir)
import           Data.Aeson                  (decode, encode, FromJSON, ToJSON)
import qualified PlutusTx
import qualified PlutusTx.Builtins
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),unless, (.))
import           Prelude                              (IO, (.), FilePath, Show, String, fromIntegral, show, div)
import           Prettyprinter.Extras                 (pretty)
import qualified Ledger                      as Plutus
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Plutus.V1.Ledger.Credential as PlutusCr
import           Plutus.V1.Ledger.Crypto     as Plutus
import qualified Cardano.Ledger.BaseTypes    as LBST (TxIx (..), CertIx (..))


-- THIS IS THE MINTING POLICY FOR THE UTILITY TOKENS & IT IS BASED
-- ON THE AMOUNT OF ADA THAT ALICE DEPOSITED TO THE LOCK SCRIPT

-- RATIO 10:1 --> FOR EVERY 10 ADA MINT 1 UTILITY TOKEN
------------------------------------------------------------
type AdaValue     = Integer 
type TokensToMint = Integer

checkRatio :: AdaValue -> TokensToMint -> Bool
checkRatio av tm = av `div` tm >=  10 

------------------------------------------------------------
------------------------------------------------------------

data TokenInfo = TokenInfo {

    currencyInfo :: AssetClass,
    currencyAmt  :: !Integer

} deriving (Show, FromJSON, ToJSON, Generic)


instance Eq TokenInfo where
    {-# INLINEABLE (==) #-}
    TokenInfo ci1 ca1 == TokenInfo ci2 ca2 = ci1 == ci2 && ca1 == ca2

PlutusTx.unstableMakeIsData ''TokenInfo


tokenPolicy :: () -> () -> PlutusV2.ScriptContext -> Bool
tokenPolicy _ _ ctx = True
  where

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    getTxInputs :: [PlutusV2.TxInInfo]
    getTxInputs = PlutusV2.txInfoInputs info

    getCurrOutputs :: [PlutusV2.TxOut]
    getCurrOutputs = PlutusV2.txInfoOutputs info
    
    getContOutputs :: [PlutusV2.TxOut]
    getContOutputs = PlutusV2.getContinuingOutputs ctx

    getTxReferenceInputs :: [PlutusV2.TxInInfo]
    getTxReferenceInputs = PlutusV2.txInfoReferenceInputs info

    hasUniqueNFT :: Bool
    hasUniqueNFT = case find (\x -> case Value.flattenValue . PlutusV2.txOutValue . PlutusV2.txInInfoResolved $ x of
                                        [(cs, tn, amt)] -> amt == 1
                                        _               -> False ) getTxInputs of
                        Nothing   -> False
                        Just _    -> True

    checkRatioAdaWithToken :: Bool
    checkRatioAdaWithToken = True 
    --   where 
    --     let mintedAmount       = PlutusV2.txInfoMint info
    --         adaOutput          = filter ( \x -> case Value.flattenValue . PlutusV2.txOutValue $ x of
    --                                               [(cs, tn, amt)] -> cs == ADAV1.adaSymbol
    --                                               _               -> False  ) getCurrOutputs
    --                     -- adaAmountDeposited = case Value.flattenValue . PlutusV2.txOutValue $ (head adaOutput) of
    --         --                        [(cs, tn, amt)] -> amt
    --         --                        _               -> 0