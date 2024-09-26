{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.VX.Contracts.StabilityPool.Common
  ( SPInteger (),
    (|-|),
    (|+|),
    (|*|),
    (|/|),
    spTruncate,
    StabilityPoolParams
      ( StabilityPoolParams,
        assetSymbol,
        stabilityPoolToken,
        snapshotEpochToScaleToSumToken,
        accountToken,
        cdpToken,
        iAssetAuthToken,
        versionRecordToken,
        collectorValHash,
        govNFT,
        accountCreateFeeLovelaces,
        accountAdjustmentFeeLovelaces,
        requestCollateralLovelaces
      ),
    StabilityPoolSnapshot
      ( StabilityPoolSnapshot,
        snapshotP,
        snapshotD,
        snapshotS,
        snapshotEpoch,
        snapshotScale
      ),
    StabilityDatum
      ( StabilityPool,
        Account,
        SnapshotEpochToScaleToSum
      ),
    StabilityPoolContent
      ( StabilityPoolContent,
        spIAsset,
        spSnapshot,
        epochToScaleToSum
      ),
    AccountContent
      ( AccountContent,
        accOwner,
        accIAsset,
        accSnapshot,
        accRequest
      ),
    SnapshotEpochToScaleToSumContent
      ( SnapshotEpochToScaleToSumContent,
        sessSnapshot,
        sessAsset
      ),
    StabilityPoolRedeemer
      ( AnnulRequest,
        LiquidateCDP,
        ProcessRequest,
        RecordEpochToScaleToSum,
        RequestAction,
        UpgradeVersion,
        action,
        request
      ),
    StabilityPoolScript,
    EpochToScaleToSum,
    AccountAction
      ( Adjust,
        Close,
        Create,
        aAmount,
        aOutputAddress,
        clOutputAddress
      ),
    ActionReturnDatum
      ( IndigoStabilityPoolAccountAdjustment,
        IndigoStabilityPoolAccountClosure,
        closedAccount,
        spentAccount
      ),
    toSPInteger,
    fromSPInteger,
    fromSPIntegerToOCD,
    snapshotPLens,
    snapshotDLens,
    snapshotSLens,
    snapshotEpochLens,
    snapshotScaleLens,
  )
where

import Control.Lens (makeLensesFor)
import GHC.Generics (Generic)
import Indigo.Common.Data.Decimal
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (divideInteger, multiplyInteger)
import PlutusTx.Prelude hiding (divide)
import Prelude qualified as P

data StabilityPoolParams = StabilityPoolParams
  { assetSymbol :: Value.CurrencySymbol,
    stabilityPoolToken :: Value.AssetClass,
    snapshotEpochToScaleToSumToken :: Value.AssetClass,
    accountToken :: Value.AssetClass,
    cdpToken :: Value.AssetClass,
    iAssetAuthToken :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass,
    collectorValHash :: ValidatorHash,
    govNFT :: Value.AssetClass,
    accountCreateFeeLovelaces :: Integer,
    accountAdjustmentFeeLovelaces :: Integer,
    requestCollateralLovelaces :: Integer
  }
  deriving (Generic, P.Show, P.Ord, P.Eq)

PlutusTx.makeLift ''StabilityPoolParams
PlutusTx.makeIsDataIndexed ''StabilityPoolParams [('StabilityPoolParams, 0)]

-- | This type is used to represent numbers at the stability pool calculations.
-- It wraps an Integer that represents a decimal with custom precision.
newtype SPInteger = SPInteger {getSPInteger :: Integer}
  deriving stock (P.Show, P.Eq, P.Ord, Generic)

PlutusTx.makeLift ''SPInteger
PlutusTx.makeIsDataIndexed ''SPInteger [('SPInteger, 0)]

spPrecision :: Integer
spPrecision = 1_000_000_000_000_000_000

-- | Multiplication with custom precision
(|*|) :: SPInteger -> SPInteger -> SPInteger
(|*|) (SPInteger a) (SPInteger b) =
  SPInteger $ divideInteger (multiplyInteger a b) spPrecision

-- | Division with custom precision
(|/|) :: SPInteger -> SPInteger -> SPInteger
(|/|) (SPInteger a) (SPInteger b) =
  SPInteger $ divideInteger (multiplyInteger a spPrecision) b

-- | Subtraction with custom precision
(|-|) :: SPInteger -> SPInteger -> SPInteger
(|-|) (SPInteger a) (SPInteger b) = SPInteger $ a - b

-- | Addition with custom precision
(|+|) :: SPInteger -> SPInteger -> SPInteger
(|+|) (SPInteger a) (SPInteger b) = SPInteger $ a + b

-- | Converts from custom precision Integer to Integer
fromSPInteger :: SPInteger -> Integer
fromSPInteger (SPInteger a) = divideInteger a spPrecision

toSPInteger :: Integer -> SPInteger
toSPInteger a = SPInteger $ multiplyInteger a spPrecision

fromSPIntegerToOCD :: SPInteger -> OnChainDecimal
fromSPIntegerToOCD (SPInteger a) =
  OnChainDecimal $ divideInteger (multiplyInteger a decimalUnit) spPrecision

spTruncate :: SPInteger -> SPInteger
spTruncate (SPInteger a)
  | a > 0 = SPInteger {getSPInteger = a}
  | otherwise = SPInteger {getSPInteger = 0}

data StabilityPoolSnapshot = StabilityPoolSnapshot
  { -- | Product snapshot - intermediate variable.
    -- Reference to product snapshot can be found
    -- in https://github.com/liquity/dev#how-deposits-and-eth-gains-are-tracked
    snapshotP :: SPInteger,
    -- | Deposit snapshot
    snapshotD :: SPInteger,
    -- | Sum snapshot - intermediate variable.
    -- Reference to sum snapshot can be found
    -- in https://github.com/liquity/dev#how-deposits-and-eth-gains-are-tracked
    snapshotS :: SPInteger,
    -- | Tracks pool emptying events.
    -- When a pool is emptied the epoch is incremented.
    snapshotEpoch :: Integer,
    -- | This is used to prevent change of `snapshotP` to 0.
    --
    -- From Liquity paper
    -- https://github.com/liquity/dev/blob/main/papers/Scalable_Reward_Distribution_with_Compounding_Stakes.pdf :
    -- * Upon a liquidation that would otherwise truncate P to 0, S is first
    -- updated as usual, for the current scale. Then, the current scale is
    -- incremented by 1, and P is updated and scaled by 1e18.
    snapshotScale :: Integer
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''StabilityPoolSnapshot
PlutusTx.makeIsDataIndexed ''StabilityPoolSnapshot [('StabilityPoolSnapshot, 0)]
makeLensesFor
  [ ("snapshotP", "snapshotPLens"),
    ("snapshotD", "snapshotDLens"),
    ("snapshotS", "snapshotSLens"),
    ("snapshotEpoch", "snapshotEpochLens"),
    ("snapshotScale", "snapshotScaleLens")
  ]
  ''StabilityPoolSnapshot

type EpochToScaleToSum = AssocMap.Map (Integer, Integer) SPInteger

data AccountAction
  = Create
  | Adjust {aAmount :: Integer, aOutputAddress :: Ledger.Address}
  | Close {clOutputAddress :: Ledger.Address}
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''AccountAction
PlutusTx.makeIsDataIndexed
  ''AccountAction
  [ ('Create, 0),
    ('Adjust, 1),
    ('Close, 2)
  ]

data StabilityPoolContent = StabilityPoolContent
  { spIAsset :: Value.TokenName,
    spSnapshot :: StabilityPoolSnapshot,
    epochToScaleToSum :: EpochToScaleToSum
  }
  deriving (P.Show, P.Eq)

PlutusTx.makeLift ''StabilityPoolContent
PlutusTx.makeIsDataIndexed ''StabilityPoolContent [('StabilityPoolContent, 0)]

data AccountContent = AccountContent
  { accOwner :: Ledger.PaymentPubKeyHash,
    accIAsset :: Value.TokenName,
    accSnapshot :: StabilityPoolSnapshot,
    accRequest :: Maybe AccountAction
  }
  deriving (P.Show, P.Eq)

PlutusTx.makeLift ''AccountContent
PlutusTx.makeIsDataIndexed ''AccountContent [('AccountContent, 0)]

data SnapshotEpochToScaleToSumContent = SnapshotEpochToScaleToSumContent
  { sessSnapshot :: EpochToScaleToSum,
    sessAsset :: Value.TokenName
  }
  deriving (P.Show, P.Eq)

PlutusTx.makeLift ''SnapshotEpochToScaleToSumContent
PlutusTx.makeIsDataIndexed ''SnapshotEpochToScaleToSumContent [('SnapshotEpochToScaleToSumContent, 0)]

data StabilityDatum
  = StabilityPool StabilityPoolContent
  | Account AccountContent
  | SnapshotEpochToScaleToSum SnapshotEpochToScaleToSumContent
  deriving stock (P.Show, P.Eq, Generic)

PlutusTx.makeLift ''StabilityDatum
PlutusTx.makeIsDataIndexed
  ''StabilityDatum
  [ ('StabilityPool, 0),
    ('Account, 1),
    ('SnapshotEpochToScaleToSum, 2)
  ]

data StabilityPoolRedeemer
  = RequestAction {action :: AccountAction}
  | ProcessRequest {request :: Ledger.TxOutRef}
  | AnnulRequest
  | LiquidateCDP
  | RecordEpochToScaleToSum
  | UpgradeVersion
  deriving stock (P.Show, Generic)

PlutusTx.makeLift ''StabilityPoolRedeemer
PlutusTx.makeIsDataIndexed
  ''StabilityPoolRedeemer
  [ ('RequestAction, 0),
    ('ProcessRequest, 1),
    ('AnnulRequest, 2),
    ('LiquidateCDP, 3),
    ('RecordEpochToScaleToSum, 4),
    ('UpgradeVersion, 5)
  ]

data ActionReturnDatum
  = IndigoStabilityPoolAccountAdjustment {spentAccount :: Ledger.TxOutRef}
  | IndigoStabilityPoolAccountClosure {closedAccount :: Ledger.TxOutRef}

PlutusTx.makeLift ''ActionReturnDatum
PlutusTx.makeIsDataIndexed
  ''ActionReturnDatum
  [ ('IndigoStabilityPoolAccountAdjustment, 0),
    ('IndigoStabilityPoolAccountClosure, 1)
  ]

data StabilityPoolScript

instance TScripts.ValidatorTypes StabilityPoolScript where
  type DatumType StabilityPoolScript = StabilityDatum
  type RedeemerType StabilityPoolScript = StabilityPoolRedeemer
