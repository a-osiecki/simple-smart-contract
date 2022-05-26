{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE BangPatterns       #-}


{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Contract where

import           Control.Monad             (forever)
import           Control.Lens
import           Data.Text
import qualified Data.Map          as Map
import           Prelude as P

import           PlutusTx.Prelude hiding ((.),($),not,(++),(<>),(+))
import qualified PlutusTx
import           Ledger.Constraints        as Constraints
import           Plutus.Contract           as Contract hiding (tell)
import           Plutus.V1.Ledger.Ada      (lovelaceValueOf)
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts

-- | Validator
data Contracting
instance Scripts.ValidatorTypes Contracting where
    type instance DatumType Contracting = ()
    type instance RedeemerType Contracting = ()

myContractInst :: Scripts.TypedValidator Contracting
myContractInst  = Scripts.mkTypedValidator @Contracting
                  $$(PlutusTx.compile [|| mkValidator ||])
                  $$(PlutusTx.compile [|| Scripts.wrapValidator ||])

myContractValidator :: Validator
myContractValidator = Scripts.validatorScript myContractInst

myContractAddress :: Ledger.Address
myContractAddress = scriptAddress myContractValidator

myRedeemer :: Redeemer
myRedeemer = Redeemer $ PlutusTx.toBuiltinData ()

-- | OffChain logic
type MySchema = Endpoint "consume" ()

endpoints :: Contract () MySchema Text ()
endpoints = forever $ handleError logError $ awaitPromise $ consumeEp
  where
    consumeEp :: Promise () MySchema Text ()
    consumeEp = endpoint @"consume" $ consumeOp

run :: Contract () MySchema Text ()
run = start >> endpoints

start :: Contract () MySchema Text ()
start = do
    logInfo @String "Starting contract..."

    let tx      =   Constraints.mustPayToTheScript () (lovelaceValueOf 2_000_000)
        lookups =   Constraints.typedValidatorLookups myContractInst
                 <> Constraints.otherScript myContractValidator

    _ <- submitTxConstraintsWith @Contracting lookups tx
    logInfo @String "Contract started"

consumeOp :: () -> Contract () MySchema Text ()
consumeOp _ = do
    ownPKH <- Contract.ownPaymentPubKeyHash
    utxos <- utxosAt myContractAddress
    (oref,outxo) <- case Map.toList utxos of
                        [] -> throwError "No UTxOs available."
                        l  -> return (P.head l)
    currTime <- currentTime

    let scriptValue = outxo ^. ciTxOutValue
        tx  =  Constraints.mustSpendScriptOutput oref myRedeemer
            <> Constraints.mustBeSignedBy ownPKH
            <> Constraints.mustValidateIn
                   (interval currTime $ currTime + windowSize)

        lkp =  Constraints.unspentOutputs (Map.fromList [(oref,outxo)])
            <> Constraints.typedValidatorLookups myContractInst
            <> Constraints.otherScript myContractValidator

    uTx <- mkTxConstraints @Contracting lkp tx
    logInfo @String $ "Unbalanced transaction: " ++ show uTx
    bTx <- balanceTx uTx
    logInfo @String $ "Balanced transaction: " ++ show bTx
    _   <- submitBalancedTx bTx
    logInfo @String "Utxo consumed succesfully"

  where
    windowSize :: POSIXTime
    windowSize = fromMilliSeconds (DiffMilliSeconds 600_000)


-- | OnChain logic
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = (not . isEmpty) $ txInterval
  where
    txInterval :: POSIXTimeRange
    !txInterval = txInfoValidRange info

    info :: TxInfo
    !info = scriptContextTxInfo ctx

