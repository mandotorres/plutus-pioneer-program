{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TicketNFT where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (CurrencySymbol, flattenValue, valueOf)
import           Plutus.V2.Ledger.Api       (BuiltinData,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts  (txInInfoResolved, TxOut( txOutValue))
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, (.), ($), (&&), (||))
import           Prelude                    (Integer, IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writePolicyToFile)

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: CurrencySymbol -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy cs oref tn () ctx = traceIfFalse "missing ticket creator nft" hasTCnft      &&
                             (traceIfFalse "UTxO not consumed"   hasUTxO                &&
                             traceIfFalse "wrong amount minted" (checkMintedAmount 1))  ||
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

    hasTCnft :: Bool
    hasTCnft = any (hasNft . txInInfoResolved) $ txInfoInputs info

    hasNft :: TxOut -> Bool
    hasNft txOut = 1 == valueOf (txOutValue txOut) cs tn

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy cs' oref' tn' = wrapPolicy $ mkNFTPolicy cs oref tn
  where
    cs :: CurrencySymbol
    cs = PlutusTx.unsafeFromBuiltinData cs'

    oref :: TxOutRef
    oref = PlutusTx.unsafeFromBuiltinData oref'

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: CurrencySymbol -> TxOutRef -> TokenName -> MintingPolicy
nftPolicy cs oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData cs)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTPolicy :: CurrencySymbol -> TxOutRef -> TokenName -> IO ()
saveNFTPolicy cs oref tn = writePolicyToFile
    (printf "assets/ticket-nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn') $
    nftPolicy cs oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

nftCurrencySymbol :: CurrencySymbol -> TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol cs oref tn = currencySymbol $ nftPolicy cs oref tn
