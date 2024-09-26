{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common data structures for CDP Script.
-}

module Indigo.VX.Contracts.CDP.Common
  ( CDPScriptParams (..),
    IAssetContent
      ( IAssetContent,
        iaName,
        iaMaintenanceRatio,
        iaLiquidationRatio,
        iaPrice,
        iaInterestOracleNft,
        iaRedemptionRatio,
        iaDebtMintingFeePercentage,
        iaLiquidationProcessingFeePercentage,
        iaStabilityPoolWithdrawalFeePercentage,
        iaFirstIAsset,
        iaNextIAsset,
        iaRedemptionReimbursementPercentage,
        iaRedemptionProcessingFeePercentage,
        iaInterestCollectorPortionPercentage
      ),
    CDPFees
      ( ActiveCDPInterestTracking,
        itLastSettled,
        itUnitaryInterestSnapshot,
        FrozenCDPAccumulatedFees,
        flaLovelacesTreasury,
        flaLovelacesIndyStakers
      ),
    CDPContent
      ( CDPContent,
        cdpOwner,
        cdpIAsset,
        cdpMintedAmount,
        cdpFeesTracking
      ),
    CDPDatum
      ( CDP,
        IAsset
      ),
    CDPRedeemer
      ( AdjustCDP,
        aCurrentTime,
        aMintedAmountChange,
        aCollateralAmountChange,
        CloseCDP,
        cCurrentTime,
        Liquidate,
        UpdateOrInsertAsset,
        UpgradeVersion,
        FreezeCDP,
        fCurrentTime,
        MergeCDPs,
        MergeAuxiliary,
        mainMergeUtxo,
        RedeemCDP,
        rCurrentTime
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
        collectorScriptHash,
        minCollateralInLovelace,
        ccBiasTime
      ),
    CDPCreatorDatum,
    CDPCreatorRedeemer
      ( CreateCDP,
        crOwner,
        crMinted,
        crCollateral,
        crCurrentTime,
        UpgradeCreatorVersion
      ),
    CDPCreatorScript,
  )
where

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
    cMinCollateralInLovelace :: Integer,
    -- | Partial redemptions have additional fee.
    -- For full redemptions this fee is not applied.
    partialRedemptionExtraFeeLovelace :: Integer,
    -- | Dictates the length of transaction validity.
    cBiasTime :: Ledger.POSIXTime,
    treasuryValHash :: ValidatorHash
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''CDPScriptParams
PlutusTx.makeIsDataIndexed ''CDPScriptParams [('CDPScriptParams, 0)]

data IAssetContent = IAssetContent
  { iaName :: Value.TokenName,
    -- | The Left value is used here only when the oracle has been delisted,
    -- otherwise the Right is used to reference the oracle.
    iaPrice :: Either OnChainDecimal OracleAssetNFT,
    iaInterestOracleNft :: Value.AssetClass,
    -- Redemption margin ratio. Represented as a percentage.
    -- I.e. 130% would be `OnChainDecimal {get_on_chain_int: 130 * decimal_unit()}`
    iaRedemptionRatio :: OnChainDecimal,
    -- | The maintenance collateral ratio of the IAsset. Represented as a percentage.
    -- This ratio is the minimum that the user can set their CDP collateral ratio to.
    -- I.e. 130% would be `OnChainDecimal {get_on_chain_int: 130 * decimal_unit()}`
    iaMaintenanceRatio :: OnChainDecimal,
    -- | The liquidation collateral ratio of the IAsset. Represented as a percentage.
    -- If a CDP is below this ratio, it is susceptible to liquidations.
    -- I.e. 130% would be `OnChainDecimal {get_on_chain_int: 130 * decimal_unit()}`
    iaLiquidationRatio :: OnChainDecimal,
    -- | The debt minting fee
    iaDebtMintingFeePercentage :: OnChainDecimal,
    -- | When a liquidation occurs, a fee should be charged on the value of the debt
    -- at time of seizure (freeze). That fee should be taken from the collateral of
    -- the CDP and sent to INDY stakers.
    iaLiquidationProcessingFeePercentage :: OnChainDecimal,
    -- | This fee is charged when withdrawing iAsset from a stability pool account.
    -- It is charged and settled in iAsset and redistributed across depositors
    -- proportionally to their deposited amount of iAsset.
    iaStabilityPoolWithdrawalFeePercentage :: OnChainDecimal,
    -- | This fee is charged to the redeemer’s redemption value. This percentage of
    -- ADA is returned to the redeemed CDPs collateral.
    iaRedemptionReimbursementPercentage :: OnChainDecimal,
    -- | This fee is charged to the redeemer’s redemption value. This percentage of
    -- ADA is returned to the redeemed CDPs collateral.
    iaRedemptionProcessingFeePercentage :: OnChainDecimal,
    iaInterestCollectorPortionPercentage :: OnChainDecimal,
    iaFirstIAsset :: Bool,
    iaNextIAsset :: Maybe Value.TokenName
  }
  deriving stock (P.Show, P.Eq, Generic)

PlutusTx.makeLift ''IAssetContent
PlutusTx.makeIsDataIndexed ''IAssetContent [('IAssetContent, 0)]

data CDPFees
  = ActiveCDPInterestTracking
      { itLastSettled :: Ledger.POSIXTime,
        itUnitaryInterestSnapshot :: Integer
      }
  | FrozenCDPAccumulatedFees
      { flaLovelacesTreasury :: Integer,
        flaLovelacesIndyStakers :: Integer
      }
  deriving (P.Show)

PlutusTx.makeLift ''CDPFees
PlutusTx.makeIsDataIndexed ''CDPFees [('ActiveCDPInterestTracking, 0), ('FrozenCDPAccumulatedFees, 1)]

data CDPContent = CDPContent
  { -- | Nothing means that the CDP is frozen and can be liquidated
    -- against the stability pool.
    cdpOwner :: Maybe Ledger.PaymentPubKeyHash,
    -- | Name of iAsset that can be minted from this position.
    cdpIAsset :: Value.TokenName,
    -- | This is 1/1e+6 of the actual unit of the iAsset.
    -- E.g. 1 BTC would be represented as minted_amount=1_000_000
    cdpMintedAmount :: Integer,
    cdpFeesTracking :: CDPFees
  }
  deriving (P.Show)

PlutusTx.makeLift ''CDPContent
PlutusTx.makeIsDataIndexed ''CDPContent [('CDPContent, 0)]

{-
Two kinds of outputs locked at the CDPScript:
1. CDP: Each CDP output represents an individual CDP.
2. IAssetDatum: Each IAssetDatum output represents an whitelisted asset with
all necessary information. Users must consume this output to open a
new CDP that can mint iAsset of that type.
-}
data CDPDatum
  = CDP CDPContent
  | IAsset IAssetContent
  deriving stock (P.Show, Generic)

PlutusTx.makeLift ''CDPDatum
PlutusTx.makeIsDataIndexed ''CDPDatum [('CDP, 0), ('IAsset, 1)]

data CDPRedeemer
  = -- | AdjustCDP to modify an existing CDP.
    AdjustCDP
      { aCurrentTime :: Ledger.POSIXTime,
        aMintedAmountChange :: Integer,
        aCollateralAmountChange :: Integer
      }
  | -- | CloseCDP to close an exisiting CDP.
    CloseCDP {cCurrentTime :: Ledger.POSIXTime}
  | RedeemCDP {rCurrentTime :: Ledger.POSIXTime}
  | -- | Freeze undercollaterized CDP position and prepare it for liquidation.
    FreezeCDP {fCurrentTime :: Ledger.POSIXTime}
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
  | -- | Liquidate to liquidate a frozen CDP.
    Liquidate
  | -- | UpdateOrInsertAsset to update iAssetDatum
    --   or use the iAsset as an element of the distributed list to insert a new iAsset.
    UpdateOrInsertAsset
  | -- | UpgradeVersion to upgrade the script.
    UpgradeVersion
  deriving stock (P.Show, Generic)

PlutusTx.makeLift ''CDPRedeemer
PlutusTx.makeIsDataIndexed
  ''CDPRedeemer
  [ ('AdjustCDP, 0),
    ('CloseCDP, 1),
    ('RedeemCDP, 2),
    ('FreezeCDP, 3),
    ('MergeCDPs, 4),
    ('MergeAuxiliary, 5),
    ('Liquidate, 6),
    ('UpdateOrInsertAsset, 7),
    ('UpgradeVersion, 8)
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
    -- | Hash of collector script
    collectorScriptHash :: ValidatorHash,
    -- | Minimum ADA allowed in a CDP
    minCollateralInLovelace :: Integer,
    -- | Dictates the length of transaction validity.
    ccBiasTime :: Ledger.POSIXTime
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

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
  = CreateCDP
      { crOwner :: Ledger.PaymentPubKeyHash,
        crMinted :: Integer,
        crCollateral :: Integer,
        crCurrentTime :: Ledger.POSIXTime
      }
  | UpgradeCreatorVersion
  deriving stock (P.Show, Generic)

PlutusTx.makeLift ''CDPCreatorRedeemer
PlutusTx.makeIsDataIndexed
  ''CDPCreatorRedeemer
  [('CreateCDP, 0), ('UpgradeCreatorVersion, 1)]

data CDPCreatorScript

instance TScripts.ValidatorTypes CDPCreatorScript where
  type DatumType CDPCreatorScript = CDPCreatorDatum
  type RedeemerType CDPCreatorScript = CDPCreatorRedeemer
