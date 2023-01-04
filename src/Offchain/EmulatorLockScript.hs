 {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Offchain.EmulatorLockScript where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import qualified Ledger.Address   as LAD
import Wallet.Emulator.Wallet
import Offchain.LockScriptWOF
import qualified Utils            as UTL

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"lock" h1 $ LockParams {
        userAddr = LAD.unPaymentPubKeyHash . UTL.unsafePaymentPubKeyHash . mockWalletAddress $ knownWallet 1,
        adaMount = 10000000
        }
    void $ waitUntilSlot 10
    callEndpoint @"unlock" h2 $ LockParams {
        userAddr = LAD.unPaymentPubKeyHash . UTL.unsafePaymentPubKeyHash . mockWalletAddress $ knownWallet 1,
        adaMount = 10000000
        }
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s