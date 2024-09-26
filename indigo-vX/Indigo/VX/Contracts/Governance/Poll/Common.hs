{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.VX.Contracts.Governance.Poll.Common
  ( VoteOption (Yes, No),
    PollStatus (VoteCount, nYes, nNo),
    PollShard (PollShard),
    PollShardContent (PollShardContent, psId, psStatus, psEndTime, psManagerAddress),
    PollManagerContent
      ( PollManagerContent,
        pId,
        pOwner,
        pContent,
        pTreasuryWithdrawal,
        pStatus,
        pVotingEndTime,
        pCreatedShards,
        pTalliedShards,
        pTotalShards,
        pProposingEndTime,
        pExpirationTime,
        pProtocolVersion,
        pMinimumQuorum
      ),
    PollManager (PollManager),
    PollParams (..),
    PollManagerParams (..),
    PollRedeemer (Vote, MergeShards),
    PollManagerRedeemer (EndPoll, CreateShards, MergeShardsManager),
    PollScript,
    PollManagerScript,
  )
where

import GHC.Generics (Generic)
import Indigo.VX.Contracts.Governance.Gov.Common (ProposalContent, TreasuryWithdrawal)
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as P

data VoteOption = Yes | No
  deriving (P.Show, Generic, P.Eq)

PlutusTx.makeLift ''VoteOption
PlutusTx.makeIsDataIndexed ''VoteOption [('Yes, 0), ('No, 1)]

data PollStatus = VoteCount {nYes :: Integer, nNo :: Integer}
  deriving (P.Show, Generic, P.Eq)

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

data PollManagerContent = PollManagerContent
  { pId :: Integer,
    pOwner :: Ledger.PaymentPubKeyHash,
    pContent :: ProposalContent,
    -- | Value proposed to be withdrawn from treasury as part of the proposal.
    pTreasuryWithdrawal :: Maybe TreasuryWithdrawal,
    pStatus :: PollStatus,
    pVotingEndTime :: Ledger.POSIXTime,
    -- number of shards already created
    pCreatedShards :: Integer,
    -- number of shards merged into PollManager
    pTalliedShards :: Integer,
    -- total number of shards
    pTotalShards :: Integer,
    pProposingEndTime :: Ledger.POSIXTime,
    pExpirationTime :: Ledger.POSIXTime,
    pProtocolVersion :: Integer,
    -- | The minimum number of votes (yes + no votes) for a proposal to be possible to pass.
    pMinimumQuorum :: Integer
  }

PlutusTx.makeLift ''PollManagerContent
PlutusTx.makeIsDataIndexed ''PollManagerContent [('PollManagerContent, 0)]

data PollManager = PollManager PollManagerContent

PlutusTx.makeLift ''PollManager
PlutusTx.makeIsDataIndexed ''PollManager [('PollManager, 0)]

data PollShardContent = PollShardContent
  { psId :: Integer,
    psStatus :: PollStatus,
    psEndTime :: Ledger.POSIXTime,
    -- | poll manager validator address
    psManagerAddress :: Ledger.Address
  }

PlutusTx.makeLift ''PollShardContent
PlutusTx.makeIsDataIndexed ''PollShardContent [('PollShardContent, 0)]

data PollShard = PollShard PollShardContent

PlutusTx.makeLift ''PollShard
PlutusTx.makeIsDataIndexed ''PollShard [('PollShard, 1)]

data PollParams = PollParams
  { pollToken :: Value.AssetClass,
    stakingToken :: Value.AssetClass,
    indyAsset :: Value.AssetClass,
    stakingValHash :: Ledger.ValidatorHash
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''PollParams
PlutusTx.makeIsDataIndexed ''PollParams [('PollParams, 0)]

data PollManagerParams = PollManagerParams
  { govNft :: Value.AssetClass,
    pollToken :: Value.AssetClass,
    upgradeToken :: Value.AssetClass,
    indyAsset :: Value.AssetClass,
    govExecuteValHash :: Ledger.ValidatorHash,
    pBiasTime :: Ledger.POSIXTime,
    -- | poll shards validator hash
    shardsValHash :: Ledger.ValidatorHash,
    treasuryValHash :: Ledger.ValidatorHash,
    -- | This is the ITD for the electorate calculation.
    initialIndyDistribution :: Integer
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''PollManagerParams
PlutusTx.makeIsDataIndexed ''PollManagerParams [('PollManagerParams, 0)]

data PollRedeemer
  = Vote VoteOption
  | MergeShards Ledger.POSIXTime V2.TxOutRef

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
