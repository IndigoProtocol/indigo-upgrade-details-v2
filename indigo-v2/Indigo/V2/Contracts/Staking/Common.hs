{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common functions, data structures for Staking Script.
-}

module Indigo.V2.Contracts.Staking.Common
  ( StakingParams
      ( StakingParams,
        stakingManagerNFT,
        stakingToken,
        indyToken,
        pollToken,
        versionRecordToken,
        collectorValHash
      ),
    RewardSnapshot (RewardSnapshot, snapshotAda),
    StakingManagerContent
      ( StakingManagerContent,
        totalStake,
        mSnapshot
      ),
    StakingPositionContent
      ( StakingPositionContent,
        owner,
        lockedAmount,
        pSnapshot
      ),
    StakingDatum
      ( StakingManager,
        StakingPosition
      ),
    StakingRedeemer
      ( CreateStakingPosition,
        UpdateTotalStake,
        Distribute,
        AdjustStakedAmount,
        Unstake,
        Lock,
        UpgradeVersion
      ),
    Staking,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude hiding (divide, toList)
import Prelude qualified as P

data StakingParams = StakingParams
  { -- | NFT identifying authentic Staking Manager output
    stakingManagerNFT :: Value.AssetClass,
    -- | Token identifying authentic staking position output
    stakingToken :: Value.AssetClass,
    -- | AssetClass of INDY token
    indyToken :: Value.AssetClass,
    -- | Token identifying authentic Poll output
    pollToken :: Value.AssetClass,
    -- | Token identifying the VersionRegistry output
    versionRecordToken :: Value.AssetClass,
    -- | Collector Script
    collectorValHash :: ValidatorHash
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''StakingParams
PlutusTx.makeIsDataIndexed ''StakingParams [('StakingParams, 0)]

data RewardSnapshot = RewardSnapshot
  { snapshotAda :: Integer
  }
  deriving (Generic, FromJSON, ToJSON, P.Show)

PlutusTx.makeLift ''RewardSnapshot
PlutusTx.makeIsDataIndexed ''RewardSnapshot [('RewardSnapshot, 0)]

data StakingManagerContent = StakingManagerContent
  { -- total INDY locked at the Staking Contract.
    totalStake :: Integer,
    mSnapshot :: RewardSnapshot
  }
  deriving (P.Show)

PlutusTx.makeLift ''StakingManagerContent
PlutusTx.makeIsDataIndexed ''StakingManagerContent [('StakingManagerContent, 0)]

data StakingPositionContent = StakingPositionContent
  { owner :: Ledger.PaymentPubKeyHash,
    -- The INDY tokens being locked for voting in the Governance Contract.
    -- It's a map:
    -- Proposal/Poll Id -> (Vote Amount, Proposal/Poll end of voting period)
    lockedAmount :: Map Integer (Integer, Ledger.POSIXTime),
    pSnapshot :: RewardSnapshot
  }
  deriving (P.Show)

PlutusTx.makeLift ''StakingPositionContent
PlutusTx.makeIsDataIndexed ''StakingPositionContent [('StakingPositionContent, 0)]

{-
There are two kinds of output being locked at this script
1. StakingManager: Store some aggregate information of the Staking Contract.
To create a new staking positon, users must consume the StakingManager output
in the transaction.
2. StakingPosition: Each output of this type represents staking account/profile
of a PubKeyHash. All INDY tokens will be stored at StakingPosition outputs.
-}
data StakingDatum
  = StakingManager StakingManagerContent
  | StakingPosition StakingPositionContent
  deriving stock (P.Show, Generic)

PlutusTx.makeLift ''StakingDatum
PlutusTx.makeIsDataIndexed
  ''StakingDatum
  [('StakingManager, 0), ('StakingPosition, 1)]

{-
Staking Redeemer Action:
1. CreateStakingPosition:
Create an individual staking profile for a new PubKeyHash.
2. UpdateTotalStake: Update total stake in StakingManager output
3. AdjustStakedAmount:
User deposit or withdraw INDY tokens to/from their staking profile.
4. Unstake: User remove their staking position
5. Lock: Lock an amount of INDY by voting in the Governance Contract
6. Unlock: Unlock locked INDY after voting period ends
-}
data StakingRedeemer
  = CreateStakingPosition Ledger.PaymentPubKeyHash
  | UpdateTotalStake
  | Distribute
  | AdjustStakedAmount Integer
  | Unstake
  | Lock
  | UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StakingRedeemer
PlutusTx.makeIsDataIndexed
  ''StakingRedeemer
  [ ('CreateStakingPosition, 0),
    ('UpdateTotalStake, 1),
    ('Distribute, 2),
    ('AdjustStakedAmount, 3),
    ('Unstake, 4),
    ('Lock, 5),
    ('UpgradeVersion, 6)
  ]

data Staking

instance TScripts.ValidatorTypes Staking where
  type DatumType Staking = StakingDatum
  type RedeemerType Staking = StakingRedeemer
