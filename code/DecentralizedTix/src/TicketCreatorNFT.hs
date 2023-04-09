{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TicketCreatorNFT where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             PubKeyHash,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts  (txSignedBy)

import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, ($), (&&), (||))
import           Prelude                    (Integer, IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writePolicyToFile)


{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: PubKeyHash -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy pkh oref tn () ctx = traceIfFalse "missing signature"   signedByOwner       &&
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

    signedByOwner :: Bool
    signedByOwner = txSignedBy info pkh

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy pkh' oref' tn' = wrapPolicy $ mkNFTPolicy pkh oref tn
  where
    pkh :: PubKeyHash
    pkh = PlutusTx.unsafeFromBuiltinData pkh'

    oref :: TxOutRef
    oref = PlutusTx.unsafeFromBuiltinData oref'

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: PubKeyHash -> TxOutRef -> TokenName -> MintingPolicy
nftPolicy pkh oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTPolicy :: PubKeyHash -> TxOutRef -> TokenName -> IO ()
saveNFTPolicy pkh oref tn = writePolicyToFile
    (printf "assets/tc-nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn') $
    nftPolicy pkh oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

nftCurrencySymbol :: PubKeyHash -> TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol pkh oref tn = currencySymbol $ nftPolicy pkh oref tn
