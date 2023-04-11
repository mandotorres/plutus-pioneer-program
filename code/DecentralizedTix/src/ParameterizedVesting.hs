{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ParameterizedVesting where

import           Plutus.V1.Ledger.Value    (assetClass, assetClassValueOf, AssetClass)
import           Plutus.V1.Ledger.Time     (getPOSIXTime)
import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (adaSymbol, adaToken, BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (valuePaidTo)
import           PlutusTx                  (applyCode, compile, liftCode,
                                            makeLift)
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (.), (<=))
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)
import           Utilities                 (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
makeLift ''VestingParams

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator params () () ctx =
    traceIfFalse "price not paid"       pricePaid       &&
    traceIfFalse "deadline not reached" deadlineReached

  where
    ada :: AssetClass 
    ada = assetClass adaSymbol adaToken

    info :: TxInfo
    info = scriptContextTxInfo ctx

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info

    pricePaid :: Bool
    pricePaid = 6 <= assetClassValueOf (valuePaidTo info (beneficiary params)) ada

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: VestingParams -> IO ()
saveVal vp = writeValidatorToFile 
  ( printf "./assets/pv-%s-%s.plutus"
    (show $ beneficiary vp)
    (show $ getPOSIXTime $ deadline vp)
  ) $ validator vp
