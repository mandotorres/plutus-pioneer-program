{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module User where

import           Plutus.V2.Ledger.Api      (BuiltinData, CurrencySymbol,
                                            MintingPolicy, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (.))
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)
import           Utilities                 (currencySymbol, wrapPolicy, writePolicyToFile)

{-# INLINABLE mkUserPolicy #-}
mkUserPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkUserPolicy pkh () ctx = traceIfFalse "missing signature" $ txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE mkWrappedUserPolicy #-}
mkWrappedUserPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedUserPolicy pkh = wrapPolicy (mkUserPolicy $ PlutusTx.unsafeFromBuiltinData pkh)

userCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
userCode = $$(PlutusTx.compile [|| mkWrappedUserPolicy ||])

userPolicy :: PubKeyHash -> MintingPolicy
userPolicy pkh = mkMintingPolicyScript $ userCode `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveUserPolicy :: PubKeyHash -> IO ()
saveUserPolicy pkh = writePolicyToFile (printf "assets/user-%s.plutus" $ show pkh) $ userPolicy pkh

userCurrencySymbol :: PubKeyHash -> CurrencySymbol
userCurrencySymbol = currencySymbol . userPolicy
