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

module LockScript where


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
import           Prelude                              (IO, (.), FilePath, Show, String, fromIntegral, show)
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


-- What does it do?
-- Stores info regarding how much ALice deposited
-- 2 Actions, Lock & UNlock the funds


data LockDatum = LockDatum {

    depositAmount :: !Integer,
    ownerKeyHash  :: Plutus.PaymentPubKeyHash

} deriving (Show, FromJSON, ToJSON, Generic)

instance Eq LockDatum where
    {-# INLINABLE (==) #-}
    LockDatum d1 p1 == LockDatum d2 p2 = p1 == p2 && d1 == d2

PlutusTx.unstableMakeIsData ''LockDatum

type AmountToUnlock = Integer
type Password       = Integer

data UserAction = Lock | Unlock AmountToUnlock Password deriving (Show, FromJSON, ToJSON, Generic)

PlutusTx.makeLift ''UserAction
PlutusTx.makeIsDataIndexed ''UserAction
 [( 'Lock , 0 ),
   ( 'Unlock, 1 )
 ]

{-# INLINEABLE lockScript #-}
lockScript :: LockDatum -> UserAction -> PlutusV2.ScriptContext -> Bool
lockScript dt rd ctx = 
    case rd of
        Lock               -> traceIfFalse "Oops not enough funds" checkMin &&
                              traceIfFalse "Not correct signature" checkSign
        Unlock amt pasw    -> traceIfFalse "Oops wrong password" checkPass &&
                              traceIfFalse "Oops mismatched amount" checkAmou &&
                              traceIfFalse "Oops not correct signature" checkSign
    
    where

      info :: PlutusV2.TxInfo
      info =  PlutusV2.scriptContextTxInfo ctx

      checkSign :: Bool
      checkSign = PlutusV2.txSignedBy info $ LAD.unPaymentPubKeyHash $ ownerKeyHash dt

      checkMin :: Bool
      checkMin = depositAmount dt >= 10000000

      checkPass :: Bool
      checkPass = case rd of 
                    Unlock amt psw -> psw == (42 :: Integer)
                    _              -> False

      checkAmou :: Bool
      checkAmou = case rd of
                   Unlock amt _ -> amt == depositAmount dt
                   _            -> False

typedValidator :: PlutusV2.Validator 
typedValidator = PLV1.mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
   where
      wrap = PSUV.V2.mkUntypedValidator lockScript

lockingScript :: PlutusV2.Script
lockingScript = PlutusV2.unValidatorScript typedValidator


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