{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module PVesting where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V1.Ledger.Value    (AssetClass, assetClass, assetClassValueOf)
import           Plutus.V2.Ledger.Api      (adaSymbol, adaToken, BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (valuePaidTo, valueSpent)
import           PlutusTx                  (applyCode, compile, liftCode,
                                            makeLift)
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (==), (<=))
import           Prelude                   (IO)
import           Utilities                 (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingParams = VestingParams
    { beneficiary :: PubKeyHash -- TODO: Possibly move to datum
    , deadline    :: POSIXTime
    }
makeLift ''VestingParams

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: AssetClass -> VestingParams -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator ac params () () ctx =
    traceIfFalse "missing user token"      hasUserToken    &&
    traceIfFalse "price not paid"          pricePaid       &&
    traceIfFalse "deadline not reached"    deadlineReached

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info

    ada :: AssetClass
    ada = assetClass adaSymbol adaToken
    
    pricePaid :: Bool
    pricePaid = 6 <= assetClassValueOf (valuePaidTo info (beneficiary params)) ada

    hasUserToken :: Bool
    hasUserToken = 1 == assetClassValueOf (valueSpent info) ac

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: AssetClass -> VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator ac params = wrapValidator $ mkParameterizedVestingValidator ac params

validator :: AssetClass -> VestingParams -> Validator
validator ac params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode ac `applyCode` liftCode params)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: AssetClass -> VestingParams -> IO ()
saveVal ac params = writeValidatorToFile "./assets/parameterized-vesting.plutus" $ validator ac params
