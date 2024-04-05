{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common functions, data structures for CDP Script.
-}

module Indigo.V1.Contracts.CDP.Common
  ( CDPScriptParams (..),
    IAsset (IAsset, iaName, iaMinRatio, iaPrice),
    CDPDatum (CDP, cdpOwner, cdpIAsset, cdpMintedAmount, IAssetDatum),
    CDPRedeemer
      ( AdjustCDP,
        CloseCDP,
        Liquidate,
        UpgradeAsset,
        UpgradeVersion,
        FreezeCDP,
        MergeCDPs,
        MergeAuxiliary,
        mainMergeUtxo
      ),
    CDPScript,
    CDPCreatorScriptParams
      ( CDPCreatorScriptParams,
        cdpCreatorNft,
        cdpAuthTk,
        cdpAssetCs,
        cdpScriptHash,
        iAssetAuthTk,
        versionRecordToken,
        minCollateralInLovelace
      ),
    CDPCreatorDatum,
    CDPCreatorRedeemer (CreateCDP, UpgradeCreatorVersion),
    CDPCreatorScript,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Indigo.Common.Contracts.Oracle.Common (OracleAssetNFT)
import Indigo.Common.Data.Decimal (OnChainDecimal)
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (divide, ratio, toList)
import Prelude qualified as P

{-
Parameters of CDP Script.
-}
data CDPScriptParams = CDPScriptParams
  { -- | Token for identifying authentic CDP output.
    cdpAuthToken :: Value.AssetClass,
    -- | Currency Symbol/Minting Policy managing the mint/burn of all iAssets.
    cdpAssetSymbol :: Value.CurrencySymbol,
    -- | Token for identifying authentic iAsset output.
    iAssetAuthToken :: Value.AssetClass,
    -- | Token for identifying authentic Stability Pool output.
    stabilityPoolAuthToken :: Value.AssetClass,
    -- | Token for identifying the version record for a protocol upgrade.
    cVersionRecordToken :: Value.AssetClass,
    -- | Token for identifying authentic iAsset output update.
    upgradeToken :: Value.AssetClass,
    -- | The validator hash for the collector script.
    collectorValHash :: ValidatorHash,
    -- | The validator hash of the stability pool script.
    spValHash :: ValidatorHash,
    -- | NFT for identifying authentic governance parameters.
    govNFT :: Value.AssetClass,
    -- | Minimum ADA allowed in a CDP
    cMinCollateralInLovelace :: Integer
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''CDPScriptParams
PlutusTx.makeIsDataIndexed ''CDPScriptParams [('CDPScriptParams, 0)]

data IAsset = IAsset
  { iaName :: Value.TokenName,
    -- | The collateral ratio of the IAsset
    iaMinRatio :: OnChainDecimal,
    -- | The Left value is used here only when the oracle has been delisted,
    -- otherwise the Right is used to reference the oracle.
    iaPrice :: Either OnChainDecimal OracleAssetNFT
  }
  deriving stock (P.Show, P.Ord, P.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''IAsset
PlutusTx.makeIsDataIndexed ''IAsset [('IAsset, 0)]

{-
Two kinds of outputs locked at the CDPScript:
1. CDP: Each CDP output represents an individual CDP.
2. IAssetDatum: Each IAssetDatum output represents an whitelisted asset with
all necessary information. Users must consume this output to open a
new CDP that can mint iAsset of that type.
-}
data CDPDatum
  = CDP
      { -- | Nothing means that the CDP is frozen and can be liquidated
        -- against the stability pool.
        cdpOwner :: Maybe Ledger.PaymentPubKeyHash,
        -- | Name of iAsset that can be minted from this position.
        cdpIAsset :: Value.TokenName,
        cdpMintedAmount :: Integer
      }
  | IAssetDatum IAsset
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CDPDatum
PlutusTx.makeIsDataIndexed ''CDPDatum [('CDP, 0), ('IAssetDatum, 1)]

data CDPRedeemer
  = -- | AdjustCDP to modify an existing CDP.
    AdjustCDP
  | -- | CloseCDP to close an exisiting CDP.
    CloseCDP
  | -- |  Liquidate to liquidate a frozen CDP.
    Liquidate
  | -- | UpgradeAsset to update iAssetDatum.
    UpgradeAsset
  | -- | UpgradeVersion to upgrade the script.
    UpgradeVersion
  | -- | Freeze undercollaterized CDP position and prepare it for liquidation.
    FreezeCDP
  | -- | Merges multiple frozen CDPs with the same iAsset into a single one.
    -- This redeemer is used with only a single UTXO at merging transaction.
    -- The other UTXOs use MegeAuxiliary redeemer.
    MergeCDPs
  | -- | Used by all other UTXOs in CDPs merging transaction.
    -- The one other is using MergeCDPs redeemer
    MergeAuxiliary
      { -- | This is the ref to UTXO called with MergeCDPs redeemer.
        mainMergeUtxo :: V2.TxOutRef
      }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CDPRedeemer
PlutusTx.makeIsDataIndexed
  ''CDPRedeemer
  [ ('AdjustCDP, 0),
    ('CloseCDP, 1),
    ('Liquidate, 2),
    ('UpgradeAsset, 3),
    ('UpgradeVersion, 4),
    ('FreezeCDP, 5),
    ('MergeCDPs, 6),
    ('MergeAuxiliary, 7)
  ]

data CDPScript

instance TScripts.ValidatorTypes CDPScript where
  type DatumType CDPScript = CDPDatum
  type RedeemerType CDPScript = CDPRedeemer

{-
Parameters of CDPCreator Script.
-}
data CDPCreatorScriptParams = CDPCreatorScriptParams
  { -- | NFT for identifying authentic CDPCreator output
    cdpCreatorNft :: Value.AssetClass,
    -- | Currency Symbol/Minting Policy managing the mint/burn of all iAssets
    cdpAssetCs :: Value.CurrencySymbol,
    -- | Token for identifying authentic CDP output
    cdpAuthTk :: Value.AssetClass,
    -- | Token for identifying authentic iAsset output
    iAssetAuthTk :: Value.AssetClass,
    -- | Token for identifying the version record for a protocol upgrade
    versionRecordToken :: Value.AssetClass,
    -- | Hash of CDP script
    cdpScriptHash :: ValidatorHash,
    -- | Minimum ADA allowed in a CDP
    minCollateralInLovelace :: Integer
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''CDPCreatorScriptParams
PlutusTx.makeIsDataIndexed
  ''CDPCreatorScriptParams
  [('CDPCreatorScriptParams, 0)]

type CDPCreatorDatum = ()

{-
This script has one sole purpose:
Factor out logic for opening new CDPs in order to reduce CDPScript's size
-}
data CDPCreatorRedeemer
  = CreateCDP Ledger.PaymentPubKeyHash Integer Integer
  | UpgradeCreatorVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CDPCreatorRedeemer
PlutusTx.makeIsDataIndexed
  ''CDPCreatorRedeemer
  [('CreateCDP, 0), ('UpgradeCreatorVersion, 1)]

data CDPCreatorScript

instance TScripts.ValidatorTypes CDPCreatorScript where
  type DatumType CDPCreatorScript = CDPCreatorDatum
  type RedeemerType CDPCreatorScript = CDPCreatorRedeemer
