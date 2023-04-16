{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TicketNFT where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (AssetClass, assetClassValueOf, CurrencySymbol, flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts  (valueSpent)                                             
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, ($), (&&), (||))
import           Prelude                    (Integer, IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writePolicyToFile)

-- TODO: Use AssetClass instead of CurrencySymbol
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: AssetClass -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy ac oref tn () ctx = traceIfFalse "missing ticket creator nft" hasUserToken      &&
                             (traceIfFalse "UTxO not consumed"   hasUTxO                    &&
                             traceIfFalse "wrong amount minted" (checkMintedAmount 1))      ||
                             traceIfFalse "wrong amount burned" (checkMintedAmount (-1))

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Integer -> Bool
    checkMintedAmount c = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == c
        _                -> False

    hasUserToken :: Bool
    hasUserToken = 1 == assetClassValueOf (valueSpent info) ac

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy ac' oref' tn' = wrapPolicy $ mkNFTPolicy ac oref tn
  where
    ac :: AssetClass
    ac = PlutusTx.unsafeFromBuiltinData ac'

    oref :: TxOutRef
    oref = PlutusTx.unsafeFromBuiltinData oref'

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: AssetClass -> TxOutRef -> TokenName -> MintingPolicy
nftPolicy ac oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ac)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTPolicy :: AssetClass -> TxOutRef -> TokenName -> IO ()
saveNFTPolicy ac oref tn = writePolicyToFile
    (printf "assets/ticket-nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn') $
    nftPolicy ac oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

nftCurrencySymbol :: AssetClass -> TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol ac oref tn = currencySymbol $ nftPolicy ac oref tn
