{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module ThirtyFiveTyped where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile)
import           PlutusTx.Prelude     (Bool, Eq ((==)), Integer, traceIfFalse,
                                       ($))
import           Prelude              (IO)
import           Utilities            (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This validator succeeds only if the redeemer is 35
--              Datum  Redeemer        ScriptContext
mk35Validator :: () -> Integer -> PlutusV2.ScriptContext -> Bool
mk35Validator _ r _ = traceIfFalse "incorrect redeemer" $ r == 35
{-# INLINABLE mk35Validator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrapValidator mk35Validator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/thirtyfivetyped.plutus" validator
