{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TicketNFT where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (AssetClass, assetClassValueOf, CurrencySymbol, flattenValue, unAssetClass)
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
import           PlutusTx.Prelude           (any, Bool (False), Eq ((==)), fst, snd,
                                             traceIfFalse, ($), (&&), (||), Ord ((>)))
import           Prelude                    (Integer, IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writePolicyToFile)

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: Integer -> AssetClass -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy seat ac oref tn () ctx = traceIfFalse "missing ticket creator nft" hasUserToken      &&
                             (traceIfFalse "UTxO not consumed"   hasUTxO                    &&
                             traceIfFalse "missing seat number" (hasSeat seat)              &&
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

    hasSeat   :: Integer -> Bool
    hasSeat i = i > 0

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy seat' ac' oref' tn' = wrapPolicy $ mkNFTPolicy seat ac oref tn
  where
    ac :: AssetClass
    ac = PlutusTx.unsafeFromBuiltinData ac'

    oref :: TxOutRef
    oref = PlutusTx.unsafeFromBuiltinData oref'

    seat :: Integer
    seat = PlutusTx.unsafeFromBuiltinData seat'

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: Integer -> AssetClass -> TxOutRef -> TokenName -> MintingPolicy
nftPolicy seat ac oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData seat)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ac)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTPolicy :: Integer ->AssetClass -> TxOutRef -> TokenName -> IO ()
saveNFTPolicy seat ac oref tn = writePolicyToFile
  (printf "assets/ticket-nft-%s-%s-%s-%s#%d-%s.plutus"
    (show seat)
    (show (fst (unAssetClass ac)))
    (tokenName (snd (unAssetClass ac)))
    (show $ txOutRefId oref)
    (txOutRefIdx oref)
    (tokenName tn)) $
  nftPolicy seat ac oref tn
  where
    tokenName :: TokenName -> String
    tokenName tn' = case unTokenName tn' of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

nftCurrencySymbol :: Integer -> AssetClass -> TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol seat ac oref tn = currencySymbol $ nftPolicy seat ac oref tn
