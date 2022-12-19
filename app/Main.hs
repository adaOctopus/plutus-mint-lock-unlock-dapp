module Main where

import Control.Exception    (throwIO)
import qualified Cardano.Api as CAPI
-- import qualified PlutusCore as PCD
import Data.String          (IsString (..))
import qualified Data.ByteString.Base16 as B16
import System.Environment   (getArgs)
import           Ledger.Value                         as Value
import qualified Ledger.Address                       as LAD
import qualified PlutusTx                             as PlutusTx
import qualified PlutusTx.Code                        as PTCD
import Data.ByteString.Lazy.Internal
import qualified         Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (pack, unpack)
import System.Environment
import Data.List
-- import qualified Data.Map as M (Map, empty, insert, lookup)
import Data.Char (ord)
-- (toBBString, toBString, OwnerInfo, MyRedeemer, writeJSON)
import qualified Utils as UTL
import qualified LockScript as LCS


main :: IO ()
main = do
    [dtFile , amt, owner] <- getArgs
    let amts = read amt :: Integer
        fwg  = UTL.unsafePaymentPubKeyHash $ UTL.unsafeReadAddress owner
        -- fwa  = LAD.unPaymentPubKeyHash $ UTL.unsafePaymentPubKeyHash $ UTL.unsafeReadAddress wAddr
        realDatum = LCS.LockDatum {

            LCS.depositAmount = amts,
            LCS.ownerKeyHash  = fwg
        }
    datumJson <- UTL.writeJSON dtFile realDatum
    print "Everything worked, datum file constructed."
    print fwg
    return ()