{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.V1.Contracts.StabilityPool.Common
  ( SPInteger (),
    (|-|),
    (|+|),
    (|*|),
    (|/|),
    StabilityPoolParams
      ( StabilityPoolParams,
        assetSymbol,
        stabilityPoolToken,
        snapshotEpochToScaleToSumToken,
        accountToken,
        cdpToken,
        versionRecordToken,
        collectorValHash,
        govNFT,
        accountCreateFeeLovelaces,
        accountAdjustmentFeeLovelaces
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
      ( StabilityPoolDatum,
        spIAsset,
        spSnapshot,
        epochToScaleToSum,
        AccountDatum,
        accOwner,
        accIAsset,
        accSnapshot,
        SnapshotEpochToScaleToSumDatum,
        sessSnapshot,
        sessAsset
      ),
    StabilityPoolRedeemer
      ( CreateAccount,
        caPkh,
        caAmount,
        AdjustAccount,
        aaDepositChange,
        LiquidateCDP,
        Close,
        SpendAccount,
        UpgradeVersion,
        RecordEpochToScaleToSum
      ),
    StabilityPoolScript,
    EpochToScaleToSum,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
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
    versionRecordToken :: Value.AssetClass,
    collectorValHash :: ValidatorHash,
    govNFT :: Value.AssetClass,
    accountCreateFeeLovelaces :: Integer,
    accountAdjustmentFeeLovelaces :: Integer
  }
  deriving (Generic, P.Show, P.Ord, ToJSON, FromJSON, P.Eq)

PlutusTx.makeLift ''StabilityPoolParams
PlutusTx.makeIsDataIndexed ''StabilityPoolParams [('StabilityPoolParams, 0)]

-- | This type is used to represent numbers at the stability pool calculations.
-- It wraps an Integer that represents a decimal with custom precision.
newtype SPInteger = SPInteger {getSPInteger :: Integer}
  deriving stock (P.Show, P.Eq, P.Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''StabilityPoolSnapshot
PlutusTx.makeIsDataIndexed ''StabilityPoolSnapshot [('StabilityPoolSnapshot, 0)]

type EpochToScaleToSum = AssocMap.Map (Integer, Integer) SPInteger

data StabilityDatum
  = StabilityPoolDatum
      { spIAsset :: Value.TokenName,
        spSnapshot :: StabilityPoolSnapshot,
        epochToScaleToSum :: EpochToScaleToSum
      }
  | AccountDatum
      { accOwner :: Ledger.PaymentPubKeyHash,
        accIAsset :: Value.TokenName,
        accSnapshot :: StabilityPoolSnapshot
      }
  | SnapshotEpochToScaleToSumDatum
      { sessSnapshot :: EpochToScaleToSum,
        sessAsset :: Value.TokenName
      }
  deriving stock (P.Show, P.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StabilityDatum
PlutusTx.makeIsDataIndexed
  ''StabilityDatum
  [ ('StabilityPoolDatum, 0),
    ('AccountDatum, 1),
    ('SnapshotEpochToScaleToSumDatum, 2)
  ]

data StabilityPoolRedeemer
  = CreateAccount {caPkh :: Ledger.PaymentPubKeyHash, caAmount :: Integer}
  | AdjustAccount
      { -- | The deposit change amount, it is negative in case of withdrawal
        -- and positive in case of deposit
        aaDepositChange :: Integer
      }
  | LiquidateCDP
  | Close
  | SpendAccount
  | RecordEpochToScaleToSum
  | UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StabilityPoolRedeemer
PlutusTx.makeIsDataIndexed
  ''StabilityPoolRedeemer
  [ ('CreateAccount, 0),
    ('AdjustAccount, 1),
    ('LiquidateCDP, 2),
    ('Close, 3),
    ('SpendAccount, 4),
    ('UpgradeVersion, 5),
    ('RecordEpochToScaleToSum, 6)
  ]

data StabilityPoolScript

instance TScripts.ValidatorTypes StabilityPoolScript where
  type DatumType StabilityPoolScript = StabilityDatum
  type RedeemerType StabilityPoolScript = StabilityPoolRedeemer
