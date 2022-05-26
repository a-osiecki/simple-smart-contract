{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings #-}

module Trace where

import Data.UUID.Types.Internal hiding (pack, unpack)
import Data.Text hiding (head)
import Data.Maybe
import Control.Monad.RWS.Strict
import PlutusTx.Numeric as PNum

import Testnet.Traces as T hiding (runTrace)

wOwner :: WalletID
wOwner = WalletID
    "d4eb5c79cdcaa2269d7a3744d1ff44de9a3c1165"
    "b909f9f656e533e20a41f948adb6b14e7b5bf81678a161f3b66f548a"

exampleTrace :: TraceM ()
exampleTrace = do
    logInfo "Starting contract"
    hOwner <- activateContractWallet wOwner "Init" ()
    checkNewTxWithTimeout wOwner 100
    logInfo "Contract started!"
    waitNSlots 1
    logInfo "Calling endpoint!"
    callEndpoint "consume" hOwner ()
    checkNewTxWithTimeout wOwner 100
    logInfo "UTxO consumed"