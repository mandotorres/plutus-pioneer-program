{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module EventNFT where

import qualified Data.ByteString.Char8       as BS8
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData
                                           , CurrencySymbol
                                           , getPOSIXTime
                                           , MintingPolicy
                                           , POSIXTime
                                           , PubKeyHash
                                           , ScriptContext (scriptContextTxInfo)
                                           , TokenName (unTokenName)
                                           , TxInInfo (txInInfoOutRef)
                                           , TxInfo (txInfoInputs, txInfoMint)
                                           , TxOutRef (txOutRefId, txOutRefIdx)
                                           , mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts  (txSignedBy)

import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any
                                           , traceIfFalse, ($), (&&), (||), Ord ((>)), not)
import           Prelude                    (Integer, IO, String, Show (show))
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol
                                           , wrapPolicy, writePolicyToFile)
import PlutusTx.Builtins (equalsByteString, emptyByteString)


{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: BuiltinByteString -> POSIXTime -> PubKeyHash -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy artist startTime pkh oref tn () ctx =
                            -- signed
                            traceIfFalse "missing signature"    signedByOwner            &&
                            -- mint
                           (traceIfFalse "UTxO not consumed"    hasUTxO                  &&
                            traceIfFalse "missing artist"      (hasArtist artist)        &&
                            traceIfFalse "missing start time"  (hasStartTime startTime)  &&
                            traceIfFalse "wrong amount minted" (checkMintedAmount 1))    ||
                            -- burn
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

    hasArtist     :: BuiltinByteString -> Bool
    hasArtist bbs = not $ equalsByteString bbs emptyByteString

    hasStartTime   :: POSIXTime -> Bool
    hasStartTime t = t > 0

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy artist' startTime' pkh' oref' tn' = wrapPolicy $ mkNFTPolicy artist startTime pkh oref tn
  where
    artist :: BuiltinByteString
    artist = PlutusTx.unsafeFromBuiltinData artist'

    startTime :: POSIXTime
    startTime = PlutusTx.unsafeFromBuiltinData startTime'

    pkh :: PubKeyHash
    pkh = PlutusTx.unsafeFromBuiltinData pkh'

    oref :: TxOutRef
    oref = PlutusTx.unsafeFromBuiltinData oref'

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: BuiltinByteString -> POSIXTime -> PubKeyHash -> TxOutRef -> TokenName -> MintingPolicy
nftPolicy artist startTime pkh oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData artist)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData startTime)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveNFTPolicy :: BuiltinByteString -> POSIXTime -> PubKeyHash -> TxOutRef -> TokenName -> IO ()
saveNFTPolicy artist startTime pkh oref tn = writePolicyToFile
    (printf "assets/event/%s-%s-%s#%d-%s.plutus"
        artist'
        (show $ getPOSIXTime startTime)
        (show $ txOutRefId oref)
        (txOutRefIdx oref)
        tn') $
    nftPolicy artist startTime pkh oref tn
  where
    artist' :: String
    artist' = case artist of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

nftCurrencySymbol :: BuiltinByteString -> POSIXTime -> PubKeyHash -> TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol artist startTime pkh oref tn = currencySymbol $ nftPolicy artist startTime pkh oref tn
