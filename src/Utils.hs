-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE DeriveAnyClass        #-}
-- {-# LANGUAGE DeriveGeneric         #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NoImplicitPrelude     #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE RankNTypes            #-}
-- {-# LANGUAGE RecordWildCards       #-}
-- {-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TemplateHaskell       #-}
-- {-# LANGUAGE TypeApplications      #-}
-- {-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Utils where

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
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           PlutusTx                             (getPir)
import           Data.Aeson                  (decode, encode, FromJSON, ToJSON)
import qualified PlutusTx
import qualified PlutusTx.Builtins
import           Prettyprinter.Extras                 (pretty)
-- MOst of these are for the string to pubkeyhash functionality
import qualified Ledger                      as Plutus
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Plutus.V1.Ledger.Credential as PlutusCr
import           Plutus.V1.Ledger.Crypto     as Plutus
import qualified Cardano.Ledger.BaseTypes    as LBST (TxIx (..), CertIx (..))



--------------------------------------------------------
--------------------------------------------------------
-- All helper functions to be used will be in this file 
-- ========================= HELPER FUNCTIONS MOVE =====

dataToScriptData :: PlutusTx.Data -> Cardano.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = Cardano.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.Map xs)      = Cardano.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs)     = Cardano.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.I n)         = Cardano.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs)        = Cardano.ScriptDataBytes bs


tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }


stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe PlutusCr.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) (LBST.TxIx y) (LBST.CertIx z))) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                       = Nothing


credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> PlutusCr.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = PlutusCr.ScriptCredential $ Plutus.ValidatorHash $ PlutusTx.Builtins.toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = PlutusCr.PubKeyCredential $ Plutus.PubKeyHash $ PlutusTx.Builtins.toBuiltin $ hashToBytes h


getCredentials :: Plutus.Address -> Maybe (Plutus.PaymentPubKeyHash, Maybe Plutus.StakePubKeyHash)
getCredentials (Plutus.Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = Plutus.PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just (StakingPtr _ _ _) -> Nothing
            Just (StakingHash h)           -> case h of
                PlutusCr.ScriptCredential _    -> Nothing
                PlutusCr.PubKeyCredential pkh' -> Just (ppkh, Just $ Plutus.StakePubKeyHash pkh')


unsafeReadAddress :: String -> Plutus.Address
unsafeReadAddress s = DM.fromMaybe (error "oops incorrect ") $ tryReadAddress s

unsafePaymentPubKeyHash :: Plutus.Address -> Plutus.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error "oops incorrect") fst $ getCredentials addr

unsafeReadTxOutRef :: String -> Plutus.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = span (/= '#') s
  in
    Plutus.TxOutRef
        { Plutus.txOutRefId  = fromString x
        , Plutus.txOutRefIdx = read y
        }


toBBString :: String -> PlutusTx.Builtins.BuiltinByteString
toBBString = PlutusTx.Builtins.encodeUtf8 . fromString

toBString :: String -> LBS.ByteString
toBString = fromString



writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . Data.Aeson.encode . Cardano.scriptDataToJson Cardano.ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData


-- $ "can't parse " ++ s ++ " as an address"
-- $ "script address " ++ show addr ++ " does not contain a payment key"

-- ========================= HELPER FUNCTIONS MOVE =====
--------------------------------------------------------
--------------------------------------------------------