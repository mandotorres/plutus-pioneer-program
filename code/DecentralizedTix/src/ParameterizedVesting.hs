{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ParameterizedVesting where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Interval  (contains)
import           Plutus.V1.Ledger.Value     (AssetClass, assetClass, assetClassValueOf, unAssetClass)
import           Plutus.V2.Ledger.Api       (adaSymbol, adaToken, BuiltinData, POSIXTime (getPOSIXTime), PubKeyHash,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName(unTokenName),
                                             TxInfo (txInfoValidRange),
                                             Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts  (valuePaidTo, valueSpent)
import           PlutusTx                   (applyCode, compile, liftCode,
                                             makeLift)
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool, fst, snd, traceIfFalse, ($), (&&), (==), (<=))
import           Prelude                    (IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, wrapValidator, writeValidatorToFile)

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
saveVal ac params = writeValidatorToFile
    (printf "./assets/parameterized-vesting-%s-%s-%s-%s.plutus"
      (show (fst (unAssetClass ac)))
      tokenName
      (show $ beneficiary params)
      (show $ getPOSIXTime (deadline params))
    ) $ 
    validator ac params
  where
    tokenName :: String
    tokenName = case unTokenName (snd (unAssetClass ac)) of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs