{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.V2.Contracts.Governance.Gov.Common
  ( ProtocolParams
      ( ProtocolParams,
        proposalDeposit,
        votingPeriod,
        effectiveDelay,
        expirationPeriod,
        collateralFeePercentage,
        totalShards,
        proposingPeriod,
        maxTreasuryLovelaceSpend,
        maxTreasuryIndySpend,
        minimumQuorum
      ),
    ProposalContent
      ( ProposeAsset,
        paAsset,
        paOracleNft,
        paRedemptionRatio,
        paMaintenanceRatio,
        paLiquidationRatio,
        paDebtMintingFeePercentage,
        paLiquidationProcessingFeePercentage,
        paStabilityPoolWithdrawalFeePercentage,
        paBaseRate,
        paRedemptionReimbursementPercentage,
        paRedemptionProcessingFeePercentage,
        ModifyAsset,
        maAsset,
        maNewAssetPrice,
        maNewRedemptionRatio,
        maNewMaintenanceRatio,
        maNewLiquidationRatio,
        maNewDebtMintingFeePercentage,
        maNewLiquidationProcessingFeePercentage,
        maNewStabilityPoolWithdrawalFeePercentage,
        maNewBaseRate,
        maNewRedemptionReimbursementPercentage,
        maNewRedemptionProcessingFeePercentage,
        ModifyProtocolParams,
        UpgradeProtocol,
        TextProposal
      ),
    GovParams (..),
    GovDatum
      ( Gov,
        currentProposal,
        currentVersion,
        protocolParams,
        iassetsCount,
        activeProposals,
        treasuryIndyWithdrawnAmt
      ),
    TreasuryWithdrawal
      ( TreasuryWithdrawal,
        destination,
        value
      ),
    GovRedeemer
      ( CreatePoll,
        cpCurrentTime,
        cpProposalOwner,
        cpContent,
        cpTreasuryWithdrawal,
        UpgradeGov,
        UpgradeVersion,
        WitnessEndPoll,
        currentTime
      ),
    GovScript,
    UpgradePaths (UpgradePaths, uId, uPaths),
    UpgradePath (UpgradePath, upgradeSymbol),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Indigo.Common.Contracts.Oracle.Common (OracleAssetNFT)
import Indigo.Common.Data.Decimal
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.Builtins qualified as PB
import PlutusTx.Prelude
import Prelude qualified as P

data ProtocolParams = ProtocolParams
  { proposalDeposit :: Integer,
    votingPeriod :: Ledger.POSIXTime,
    effectiveDelay :: Ledger.POSIXTime,
    expirationPeriod :: Ledger.POSIXTime,
    collateralFeePercentage :: OnChainDecimal,
    -- time window for creating all voting shards
    proposingPeriod :: Ledger.POSIXTime,
    -- total numer of shards used for voting
    totalShards :: Integer,
    -- | The minimum number of votes (yes + no votes) for a proposal to be possible to pass.
    minimumQuorum :: Integer,
    -- | max lovelace spend from treasury
    maxTreasuryLovelaceSpend :: Integer,
    -- | max INDY spend from treasury
    maxTreasuryIndySpend :: Integer
  }
  deriving stock (P.Show, Generic, P.Eq, P.Ord)
  deriving anyclass (ToJSON, FromJSON)

instance Eq ProtocolParams where
  {-# INLINEABLE (==) #-}
  (==) p1 p2 =
    proposalDeposit p1 == proposalDeposit p2
      && votingPeriod p1 == votingPeriod p2
      && effectiveDelay p1 == effectiveDelay p2
      && expirationPeriod p1 == expirationPeriod p2
      && proposingPeriod p1 == proposingPeriod p2
      && totalShards p1 == totalShards p2

PlutusTx.makeLift ''ProtocolParams
PlutusTx.makeIsDataIndexed ''ProtocolParams [('ProtocolParams, 0)]

-- | An upgrade proposal may need to contain more evidence than just the
-- CurrencySymbol, like the new validator. In that case the additional
-- evidence can be stored in new fields of the 'UpgradePath' data type.
newtype UpgradePath = UpgradePath {upgradeSymbol :: Value.CurrencySymbol}
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''UpgradePath
PlutusTx.makeIsDataIndexed ''UpgradePath [('UpgradePath, 0)]

data UpgradePaths = UpgradePaths
  { uId :: Integer,
    uPaths :: Map ValidatorHash UpgradePath
  }
  deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''UpgradePaths
PlutusTx.makeIsDataIndexed ''UpgradePaths [('UpgradePaths, 0)]

data ProposalContent
  = ProposeAsset
      { paAsset :: Value.TokenName,
        paOracleNft :: OracleAssetNFT,
        paRedemptionRatio :: OnChainDecimal,
        paMaintenanceRatio :: OnChainDecimal,
        paLiquidationRatio :: OnChainDecimal,
        paDebtMintingFeePercentage :: OnChainDecimal,
        paLiquidationProcessingFeePercentage :: OnChainDecimal,
        paStabilityPoolWithdrawalFeePercentage :: OnChainDecimal,
        paRedemptionReimbursementPercentage :: OnChainDecimal,
        paRedemptionProcessingFeePercentage :: OnChainDecimal,
        -- | Initial rate for CDP interest.
        paBaseRate :: Maybe OnChainDecimal
      }
  | ModifyAsset
      { maAsset :: Value.TokenName,
        maNewAssetPrice :: Either OnChainDecimal OracleAssetNFT,
        maNewRedemptionRatio :: OnChainDecimal,
        maNewMaintenanceRatio :: OnChainDecimal,
        maNewLiquidationRatio :: OnChainDecimal,
        maNewDebtMintingFeePercentage :: OnChainDecimal,
        maNewLiquidationProcessingFeePercentage :: OnChainDecimal,
        maNewStabilityPoolWithdrawalFeePercentage :: OnChainDecimal,
        maNewRedemptionReimbursementPercentage :: OnChainDecimal,
        maNewRedemptionProcessingFeePercentage :: OnChainDecimal,
        -- | New rate for CDP interest.
        maNewBaseRate :: Maybe OnChainDecimal
      }
  | ModifyProtocolParams ProtocolParams
  | UpgradeProtocol UpgradePaths
  | TextProposal PB.BuiltinByteString
  deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''ProposalContent
PlutusTx.makeIsDataIndexed
  ''ProposalContent
  [ ('ProposeAsset, 0),
    ('ModifyAsset, 1),
    ('ModifyProtocolParams, 2),
    ('UpgradeProtocol, 3),
    ('TextProposal, 4)
  ]

{- Parameters of the Gov Script -}
data GovParams = GovParams
  { govNFT :: Value.AssetClass,
    pollToken :: Value.AssetClass,
    upgradeToken :: Value.AssetClass,
    indyAsset :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass,
    pollManagerValHash :: ValidatorHash,
    gBiasTime :: Ledger.POSIXTime,
    -- | Unique token created for treasury.
    daoIdentityToken :: Value.AssetClass,
    -- | Auth token authenticating iasset UTXO.
    iassetAuthToken :: Ledger.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''GovParams
PlutusTx.makeIsDataIndexed ''GovParams [('GovParams, 0)]

data GovDatum = Gov
  { -- | number of opened Proposal. Use for indexing Proposal
    currentProposal :: Integer,
    protocolParams :: ProtocolParams,
    -- | current version of the protocol, starting at 0
    currentVersion :: Integer,
    -- | Total iassets count. Even delisted assets count towards this total.
    iassetsCount :: Integer,
    activeProposals :: Integer,
    -- | The amount of INDY that was withdrawn from treasury up to the
    -- amount of INDY locked in treasury at protocol initialisation.
    treasuryIndyWithdrawnAmt :: Integer
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''GovDatum
PlutusTx.makeIsDataIndexed ''GovDatum [('Gov, 0)]

{- Treasury Withdrawl -}
data TreasuryWithdrawal = TreasuryWithdrawal
  { destination :: Ledger.Address,
    value :: [(Value.CurrencySymbol, Value.TokenName, Integer)]
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''TreasuryWithdrawal
PlutusTx.makeIsDataIndexed ''TreasuryWithdrawal [('TreasuryWithdrawal, 0)]

data GovRedeemer
  = CreatePoll
      { cpCurrentTime :: Ledger.POSIXTime,
        cpProposalOwner :: Ledger.PaymentPubKeyHash,
        cpContent :: ProposalContent,
        -- | Value proposed to be withdrawn from treasury as part of the proposa
        cpTreasuryWithdrawal :: Maybe TreasuryWithdrawal
      }
  | WitnessEndPoll
      { -- | Should be the same as the Poll manager's EndPoll uses.
        currentTime :: Ledger.POSIXTime
      }
  | UpgradeGov
  | UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''GovRedeemer
PlutusTx.makeIsDataIndexed
  ''GovRedeemer
  [ ('CreatePoll, 0),
    ('WitnessEndPoll, 1),
    ('UpgradeGov, 2),
    ('UpgradeVersion, 3)
  ]

data GovScript

instance TScripts.ValidatorTypes GovScript where
  type DatumType GovScript = GovDatum
  type RedeemerType GovScript = GovRedeemer
