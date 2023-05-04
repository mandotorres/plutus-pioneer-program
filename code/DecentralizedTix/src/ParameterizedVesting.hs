{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ParameterizedVesting where

import           Data.Maybe                (fromJust)
import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Interval  (contains)
import           Plutus.V1.Ledger.Value     (AssetClass, assetClass, assetClassValueOf, unAssetClass)
import           Plutus.V2.Ledger.Api       (adaSymbol, adaToken, BuiltinData, POSIXTime, PubKeyHash
                                           , ScriptContext (scriptContextTxInfo)
                                           , TokenName(unTokenName)
                                           , TxInfo (txInfoValidRange)
                                           , Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts  (valuePaidTo, valueSpent)
import           PlutusTx                   (applyCode, compile, liftCode
                                           , unstableMakeIsData)
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool, fst, snd, traceIfFalse, ($), (&&), (==), (<=))
import           Prelude                    (IO, Show (show), String, Integer)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, posixTimeFromIso8601, printDataToJSON
                                           , wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary      :: PubKeyHash
    , dateAvailable    :: POSIXTime
    , price            :: Integer
    }
unstableMakeIsData ''VestingDatum

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: AssetClass -> VestingDatum -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator ac dat () ctx =
    traceIfFalse "missing user token"              hasUserToken                 &&
    traceIfFalse "beneficiary not paid in full"    beneficiaryPaid              &&
    traceIfFalse "ticket is not yet available"     availableDateReached

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    availableDateReached :: Bool
    availableDateReached = contains (from $ dateAvailable dat) $ txInfoValidRange info

    ada :: AssetClass
    ada = assetClass adaSymbol adaToken

    beneficiaryPaid :: Bool
    beneficiaryPaid = price dat <= assetClassValueOf (valuePaidTo info (beneficiary dat)) ada

    hasUserToken :: Bool
    hasUserToken = 1 == assetClassValueOf (valueSpent info) ac

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: AssetClass -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator ac = wrapValidator $ mkParameterizedVestingValidator ac

validator :: AssetClass -> Validator
validator ac = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode ac)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: AssetClass -> IO ()
saveVal ac = writeValidatorToFile
    (printf "./assets/parameterized-vesting-%s-%s.plutus"
      (show (fst (unAssetClass ac)))
      tokenName
    ) $
    validator ac
  where
    tokenName :: String
    tokenName = case unTokenName (snd (unAssetClass ac)) of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

printVestingDatumJSON :: PubKeyHash -> String -> Integer -> IO ()
printVestingDatumJSON pkh time cost = printDataToJSON $ VestingDatum
    { beneficiary      = pkh
    , dateAvailable    = fromJust $ posixTimeFromIso8601 time
    , price            = cost
    }