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
import           PlutusTx                             (getPir)
import           Data.Aeson                  (decode, encode, FromJSON, ToJSON)
import qualified PlutusTx
import qualified PlutusTx.Builtins
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),unless, (.))
import           Prelude                              (IO, (.), FilePath, Show, String, fromIntegral, show, div)
import           Prettyprinter.Extras                 (pretty)
import qualified PlutusTx.Prelude            as PPP (divide)
import qualified Ledger                      as Plutus
import qualified Plutus.V1.Ledger.Address    as PAD


-- THIS IS THE MINTING POLICY FOR THE UTILITY TOKENS & IT IS BASED
-- ON THE AMOUNT OF ADA THAT ALICE DEPOSITED TO THE LOCK SCRIPT

-- RATIO 10:1 --> FOR EVERY 10 ADA MINT 1 UTILITY TOKEN
------------------------------------------------------------
{-# INLINEABLE tokenPolicy #-}
tokenPolicy ::  Plutus.Address -> () -> PlutusV2.ScriptContext -> Bool
tokenPolicy lca _ ctx = True


policy :: Plutus.Address -> PlutusV2.MintingPolicy 
policy adr = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode adr
   where
      wrap adr' = PSU.V2.mkUntypedMintingPolicy $ tokenPolicy adr'

policyScript :: Plutus.Address -> PlutusV2.Script
policyScript = PlutusV2.unMintingPolicyScript . policy

-- SHORTBYTESTRING
scriptSBSV2 :: Plutus.Address -> SBS.ShortByteString
scriptSBSV2 adr = SBS.toShort . LBS.toStrict $ serialise $ policyScript adr


-- FINAL SERIALIZATION STEP
serialisedScriptV2 :: Plutus.Address -> PlutusScript PlutusScriptV2
serialisedScriptV2 adr = PlutusScriptSerialised $ scriptSBSV2 adr

writeSerialisedScriptV2 :: Plutus.Address -> IO ()
writeSerialisedScriptV2 adr = void $ writeFileTextEnvelope "scripts/utility-token.plutus" Nothing (serialisedScriptV2 adr)