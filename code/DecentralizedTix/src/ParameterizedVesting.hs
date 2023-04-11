{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ParameterizedVesting where

import           Plutus.V1.Ledger.Value    (AssetClass, assetClass, assetClassValueOf, unAssetClass, valueOf)
import           Plutus.V1.Ledger.Time     (getPOSIXTime)
import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (adaSymbol, adaToken, BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoInputs, txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txInInfoResolved, TxOut, txOutValue, valuePaidTo)
import           PlutusTx                  (applyCode, compile, liftCode,
                                            makeLift)
import           PlutusTx.Prelude          (any, Bool, Eq ((==)), traceIfFalse, ($), (.), (&&), (<=),
                                            fst, snd)
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)
import           Utilities                 (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show
makeLift ''VestingParams

{-# INLINABLE mkParameterizedVestingValidator #-}
mkParameterizedVestingValidator :: VestingParams -> AssetClass -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator params ac () () ctx =
    traceIfFalse "missing ticket creator nft" hasTCnft  &&
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

    hasTCnft :: Bool
    hasTCnft = any (hasNft . txInInfoResolved) $ txInfoInputs info

    hasNft :: TxOut -> Bool
    hasNft txOut = 1 == valueOf (txOutValue txOut) (fst (unAssetClass ac)) (snd (unAssetClass ac))

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: VestingParams -> AssetClass -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator params ac = wrapValidator $ mkParameterizedVestingValidator params ac

-- {-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
-- mkWrappedParameterizedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkWrappedParameterizedVestingValidator params' ac' = wrapValidator $ mkParameterizedVestingValidator params ac
--   where
--     ac :: AssetClass
--     ac = unsafeFromBuiltinData ac'

--     params :: VestingParams
--     params = params'

-- nftCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
-- nftCode = $$(compile [|| mkWrappedParameterizedVestingValidator ||])

validator :: VestingParams -> AssetClass -> Validator
validator params ac = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params `applyCode` liftCode ac)
-- validator params ac = mkValidatorScript $
--   nftCode
--     `applyCode` liftCode (toBuiltinData params)
--     `applyCode` liftCode (toBuiltinData ac)
-- validator params ac = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)
    -- `applyCode` liftCode (toBuiltinData ac)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: VestingParams -> AssetClass -> IO ()
saveVal params ac = writeValidatorToFile
  ( printf "./assets/pv-%s-%s.plutus"
    (show $ beneficiary params)
    (show $ getPOSIXTime $ deadline params)
  ) $ validator params ac
