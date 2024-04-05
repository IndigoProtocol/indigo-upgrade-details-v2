{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.V1.Contracts.Governance.Poll.Common
  ( VoteOption (Yes, No),
    PollStatus (VoteCount, nYes, nNo),
    PollShard (PollShard, psId, psStatus, psEndTime, psManagerAddress),
    PollManager
      ( PollManager,
        pId,
        pOwner,
        pContent,
        pStatus,
        pEndTime,
        pCreatedShards,
        pTalliedShards,
        pTotalShards,
        pProposeEndTime,
        pExpirationTime,
        pProtocolVersion
      ),
    DistributionSchedule
      ( MkDistributionSchedule,
        λM_spd,
        z_spd,
        λM_lpd,
        z_lpd,
        λM_ipd,
        z_ipd,
        λM_tv,
        z_tv
      ),
    PollParams (..),
    PollManagerParams (..),
    PollRedeemer (Vote, MergeShards),
    PollManagerRedeemer (EndPoll, CreateShards, MergeShardsManager),
    PollScript,
    PollManagerScript,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Indigo.Common.Data.Decimal qualified as OCD
import Indigo.V1.Contracts.Governance.Gov.Common (ProposalContent)
import Indigo.V1.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as P

data VoteOption = Yes | No
  deriving (P.Show, Generic, ToJSON, FromJSON, P.Eq)

PlutusTx.makeLift ''VoteOption
PlutusTx.makeIsDataIndexed ''VoteOption [('Yes, 0), ('No, 1)]

data PollStatus = VoteCount {nYes :: Integer, nNo :: Integer}
  deriving (P.Show, Generic, ToJSON, FromJSON, P.Eq)

PlutusTx.makeLift ''PollStatus
PlutusTx.makeIsDataIndexed ''PollStatus [('VoteCount, 0)]

instance Eq PollStatus where
  {-# INLINEABLE (==) #-}
  (VoteCount y1 n1) == (VoteCount y2 n2) =
    y1 == y2 && n1 == n2

instance Semigroup PollStatus where
  {-# INLINEABLE (<>) #-}
  ps1 <> ps2 =
    VoteCount
      { nYes = nYes ps1 + nYes ps2,
        nNo = nNo ps1 + nNo ps2
      }

instance Monoid PollStatus where
  {-# INLINEABLE mempty #-}
  mempty = VoteCount {nYes = 0, nNo = 0}

data PollManager = PollManager
  { pId :: Integer,
    pOwner :: Ledger.PaymentPubKeyHash,
    pContent :: ProposalContent,
    pStatus :: PollStatus,
    pEndTime :: Ledger.POSIXTime,
    -- number of shards already created
    pCreatedShards :: Integer,
    -- number of shards merged into PollManager
    pTalliedShards :: Integer,
    -- total number of shards
    pTotalShards :: Integer,
    pProposeEndTime :: Ledger.POSIXTime,
    pExpirationTime :: Ledger.POSIXTime,
    pProtocolVersion :: Integer
  }

PlutusTx.makeLift ''PollManager
PlutusTx.makeIsDataIndexed ''PollManager [('PollManager, 0)]

data PollShard = PollShard
  { psId :: Integer,
    psStatus :: PollStatus,
    psEndTime :: Ledger.POSIXTime,
    -- | poll manager validator address
    psManagerAddress :: Spooky.Address
  }

PlutusTx.makeLift ''PollShard
PlutusTx.makeIsDataIndexed ''PollShard [('PollShard, 1)]

-- |  `λM` is the distribution map of the vesting schedule
--    `z` is the delay of a distribution schedule.
--    `λM` values are representing a map using the list since
--    all the values in the map have the same spaces.
data DistributionSchedule = MkDistributionSchedule
  { λM_spd :: [OCD.OnChainDecimal],
    z_spd :: Integer,
    λM_lpd :: [OCD.OnChainDecimal],
    z_lpd :: Integer,
    λM_ipd :: [OCD.OnChainDecimal],
    z_ipd :: Integer,
    λM_tv :: OCD.OnChainDecimal,
    z_tv :: Integer
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''DistributionSchedule
PlutusTx.makeIsDataIndexed ''DistributionSchedule [('MkDistributionSchedule, 0)]

data PollParams = PollParams
  { pollToken :: Spooky.AssetClass,
    stakingToken :: Spooky.AssetClass,
    indyAsset :: Spooky.AssetClass,
    stakingValHash :: Spooky.ValidatorHash
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''PollParams
PlutusTx.makeIsDataIndexed ''PollParams [('PollParams, 0)]

data PollManagerParams = PollManagerParams
  { govNFT :: Value.AssetClass,
    pollToken :: Value.AssetClass,
    upgradeToken :: Value.AssetClass,
    stakingToken :: Value.AssetClass,
    indyAsset :: Value.AssetClass,
    govExecuteValHash :: Ledger.ValidatorHash,
    stakingValHash :: Ledger.ValidatorHash,
    pBiasTime :: Ledger.POSIXTime,
    -- | poll shards validator hash
    shardsValHash :: Ledger.ValidatorHash,
    treasuryValHash :: Ledger.ValidatorHash,
    -- | This is the ITD for the electorate calculation.
    initialIndyDistribution :: Integer,
    -- | This is the t parameter in the electorate calculation.
    totalINDYSupply :: Integer,
    -- | distribution maps for each vesting schedule
    distributionSchedule :: DistributionSchedule
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''PollManagerParams
PlutusTx.makeIsDataIndexed ''PollManagerParams [('PollManagerParams, 0)]

data PollRedeemer
  = Vote VoteOption
  | MergeShards Ledger.POSIXTime Spooky.TxOutRef

PlutusTx.makeLift ''PollRedeemer
PlutusTx.makeIsDataIndexed ''PollRedeemer [('Vote, 0), ('MergeShards, 1)]

data PollManagerRedeemer
  = EndPoll Ledger.POSIXTime
  | CreateShards Ledger.POSIXTime
  | MergeShardsManager Ledger.POSIXTime

PlutusTx.makeLift ''PollManagerRedeemer
PlutusTx.makeIsDataIndexed
  ''PollManagerRedeemer
  [('EndPoll, 0), ('CreateShards, 1), ('MergeShardsManager, 2)]

data PollScript

data PollManagerScript

instance TScripts.ValidatorTypes PollScript where
  type DatumType PollScript = PollShard
  type RedeemerType PollScript = PollRedeemer

instance TScripts.ValidatorTypes PollManagerScript where
  type DatumType PollManagerScript = PollManager
  type RedeemerType PollManagerScript = PollManagerRedeemer
