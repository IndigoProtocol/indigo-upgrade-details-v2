module Main (main) where

import Data.ByteString.Base16
import qualified Data.ByteString as BS

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model.V2 (validatorHash)
import Indigo.V1.Utils.Spooky qualified as Spooky

import Indigo.V1.Contracts.CDP.Common as V1CDP
import Indigo.V1.Contracts.Collector.Common as V1Collector
import Indigo.V1.Contracts.Governance.Execute.Common as V1Execute
import Indigo.V1.Contracts.Governance.Gov.Common as V1Gov
import Indigo.V1.Contracts.Governance.Poll.Common as V1PollManager (PollManagerParams (..)) 
import Indigo.V1.Contracts.Governance.Poll.Common as V1PollShard (PollParams (..)) 
import Indigo.V1.Contracts.Governance.Poll.Common (DistributionSchedule (..))
import Indigo.V1.Contracts.Governance.VersionRegistry.Common as V1VersionRegistry
import Indigo.V1.Contracts.StabilityPool.Common as V1StabilityPool
import Indigo.V1.Contracts.Staking.Common as V1Staking
import Indigo.V1.Contracts.Treasury.Common as V1Treasury

import Tests.V1.Lib.CDP.Script as V1CDPScript
import Tests.V1.Lib.Collector.Script as V1CollectorScript
import Tests.V1.Lib.Governance.Script as V1GovScript
import Tests.V1.Lib.StabilityPool.Script as V1StabilityPoolScript
import Tests.V1.Lib.Staking.Script as V1StakingScript
import Tests.V1.Lib.Treasury.Script as V1TreasuryScript

import Indigo.V2.Contracts.CDP.Common as V2CDP
import Indigo.V2.Contracts.Collector.Common as V2Collector
import Indigo.V2.Contracts.Governance.Gov.Common as V2Gov
import Indigo.V2.Contracts.Governance.Execute.Common as V2Execute
import Indigo.V2.Contracts.Governance.Poll.Common as V2Poll
import Indigo.V2.Contracts.Governance.VersionRegistry.Common as V2VersionRegistry
import Indigo.V2.Contracts.StabilityPool.Common as V2StabilityPool
import Indigo.V2.Contracts.Staking.Common as V2Staking
import Indigo.V2.Contracts.Treasury.Common as V2Treasury

import Tests.V2.Lib.CDP.Script as V2CDPScript
import Tests.V2.Lib.Collector.Script as V2CollectorScript
import Tests.V2.Lib.Governance.Script as V2GovScript
import Tests.V2.Lib.StabilityPool.Script as V2StabilityPoolScript
import Tests.V2.Lib.Staking.Script as V2StakingScript
import Tests.V2.Lib.Treasury.Script as V2TreasuryScript

import Tests.Upgrades.V1.Lib.CDP.Script as UpgradesCDPScript
import Tests.Upgrades.V1.Lib.Collector.Script as UpgradesCollectorScript
import Tests.Upgrades.V1.Lib.Governance.Script as UpgradesGovernanceScript
import Tests.Upgrades.V1.Lib.StabilityPool.Script as UpgradesStabilityPoolScript
import Tests.Upgrades.V1.Lib.Staking.Script as UpgradesStakingScript
import Tests.Upgrades.V1.Lib.Treasury.Script as UpgradesTreasuryScript

import Indigo.Common.Data.Decimal (OnChainDecimal (OnChainDecimal))

import Prelude

-- This script is meant to be a replica for comparison: 
-- https://github.com/IndigoProtocol/smart-contracts-offchain/blob/v2/src/Indigo/Upgrade/Common.purs

-- |-----------------------------------------------------------------------
-- | Constants
-- |-----------------------------------------------------------------------

-- DAO Identity Token is presented as a constant here since there is only one authentic DAO identity token in mainnet.
-- DAO Identity Token: https://cexplorer.io/asset/asset1stsxrrs5nwnad7y3d45yzk6l9qka9pjzuvpjtd
daoIdentityTokenV2 :: Value.AssetClass
daoIdentityTokenV2 = mkAssetClass "07d3770b2ddfa1cb35d32fab2446731f50efd9229ae4b1180552558d" "DAO"

maxInterestPeriodsV2 :: Integer
maxInterestPeriodsV2 = 40

cdpBiasTimeV2 :: Ledger.POSIXTime
cdpBiasTimeV2 = Ledger.POSIXTime 1_200_000

partialRedemptionExtraFeeLovelaceV2 :: Integer
partialRedemptionExtraFeeLovelaceV2 = 10_000_000

requestCollateralLovelacesV2 :: Integer
requestCollateralLovelacesV2 = 9_000_000

-- versionRecordTokenName :: Value.TokenName
-- versionRecordTokenName = "VERSION_RECORD"

-- |-----------------------------------------------------------------------
-- | V1 Parameters
-- | These are taken from the System Params file:
-- | https://config.indigoprotocol.io/mainnet-network-with-vesting.json
-- |-----------------------------------------------------------------------

cdpCreatorV1Params :: V1CDP.CDPCreatorScriptParams
cdpCreatorV1Params =
  V1CDP.CDPCreatorScriptParams
    { cdpCreatorNft = mkAssetClass "735b37149eb0c2a5fb590bd60e39fe90ae3a96b6065b05d7aca99ebb" "CDP_CREATOR"
    , cdpAssetCs = mkCurrencySymbol "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880"
    , cdpAuthTk = mkAssetClass "708f5e6d597fc038d09a738d7be32edd6ea779d6feb32a53668d9050" "CDP"
    , iAssetAuthTk = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , versionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    , cdpScriptHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6"
    , minCollateralInLovelace = 10_000_000
    }

cdpV1Params :: V1CDP.CDPScriptParams
cdpV1Params =
  V1CDP.CDPScriptParams
    { cdpAuthToken = mkAssetClass "708f5e6d597fc038d09a738d7be32edd6ea779d6feb32a53668d9050" "CDP"
    , cdpAssetSymbol = mkCurrencySymbol "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880"
    , iAssetAuthToken = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , stabilityPoolAuthToken = mkAssetClass "3f28fb7d6c40468262dffb1c3adb568b342499826b664d940085d022" "STABILITY_POOL"
    , cVersionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    , upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , collectorValHash = "de1585e046f16fdf79767300233c1affbe9d30340656acfde45e9142"
    , spValHash = "a473cb8eb0b61c03b8696fceab1c1a89fa3ec834572850e7c2abe783"
    , govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , cMinCollateralInLovelace = 10_000_000
    }

collectorV1Params :: V1Collector.CollectorScriptParams
collectorV1Params = 
  V1Collector.CollectorScriptParams
    { stakingManagerNFT = Spooky.toSpookyAssetClass $ mkAssetClass "24b458412c2a7f9acb9c53c7ec4325b36806912ed56d2f91bfcf4d26" "STAKING_MANAGER_NFT"
    , stakingToken = Spooky.toSpookyAssetClass $ mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , versionRecordToken = Spooky.toSpookyAssetClass $ mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    }

govV1Params :: V1Gov.GovParams
govV1Params =
  V1Gov.GovParams 
    { govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , indyAsset = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , versionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    , pollManagerValHash = "127912493e7b3c36a68aa4d8916fb9076b44faa5e15142486e317af4"
    , gBiasTime = 1200000
    }

stabilityPoolV1Params :: V1StabilityPool.StabilityPoolParams
stabilityPoolV1Params =
  V1StabilityPool.StabilityPoolParams
    { assetSymbol = mkCurrencySymbol "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880"
    , stabilityPoolToken = mkAssetClass "3f28fb7d6c40468262dffb1c3adb568b342499826b664d940085d022" "STABILITY_POOL"
    , snapshotEpochToScaleToSumToken = mkAssetClass "98ebc5df52729be30f9983cc878acba0a1cabbf2fd329b5a16214720" "SP_EPOCH"
    , accountToken = mkAssetClass "443c51db609bba8b2aa4c8af248bf797cbfcfa1e413c443296a50813" "SP_ACCOUNT"
    , cdpToken = mkAssetClass "708f5e6d597fc038d09a738d7be32edd6ea779d6feb32a53668d9050" "CDP"
    , versionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    , collectorValHash = "de1585e046f16fdf79767300233c1affbe9d30340656acfde45e9142"
    , govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , accountCreateFeeLovelaces = 5000000
    , accountAdjustmentFeeLovelaces = 1000000
    }

stakingV1Params :: V1Staking.StakingParams
stakingV1Params =
  V1Staking.StakingParams
    { stakingManagerNFT = mkAssetClass "24b458412c2a7f9acb9c53c7ec4325b36806912ed56d2f91bfcf4d26" "STAKING_MANAGER_NFT"
    , stakingToken = mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , indyToken = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , versionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    , collectorValHash = "de1585e046f16fdf79767300233c1affbe9d30340656acfde45e9142"
    }

treasuryV1Params :: V1Treasury.TreasuryScriptParams
treasuryV1Params = 
  V1Treasury.MkTreasuryScriptParams
    { versionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    }

versionRecordV1Params :: V1VersionRegistry.VersionRecordParams
versionRecordV1Params = 
  V1VersionRegistry.VersionRecordParams
    { upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    }
    
pollManagerV1Params :: V1PollManager.PollManagerParams
pollManagerV1Params =
  V1PollManager.PollManagerParams
    { govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT" 
    , pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , stakingToken = mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , stakingValHash = "54e8d424816f5bbd423353009ff7c31ada2cbdefe651175014117f46"
    , indyAsset = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , govExecuteValHash = "21a8e689ae9628b84e1767e5871c8a2d30f430579bc14fd863ffe720"
    , pBiasTime = 1200000
    , shardsValHash = "c1af46643698bc73f9857a0cc21ec949ce6919974b435d057dc448b8"
    , treasuryValHash = "8f052a0be1d00e900a18a007382606d4eb0b262836c33de03900ad39"
    , initialIndyDistribution = 1575000000000
    , totalINDYSupply = 35000000000000
    , distributionSchedule = MkDistributionSchedule
      { λM_spd =
          [ OnChainDecimal 60_000,
            OnChainDecimal 70_000,
            OnChainDecimal 80_000,
            OnChainDecimal 90_000,
            OnChainDecimal 100_000
          ],
        z_spd = 2,
        λM_lpd =
          [ OnChainDecimal 10_000,
            OnChainDecimal 20_000,
            OnChainDecimal 30_000,
            OnChainDecimal 40_000,
            OnChainDecimal 50_000
          ],
        z_lpd = 6,
        λM_ipd =
          [ OnChainDecimal 5_000,
            OnChainDecimal 7_500,
            OnChainDecimal 10_000,
            OnChainDecimal 12_500,
            OnChainDecimal 15_000
          ],
        z_ipd = 3,
        λM_tv = OnChainDecimal 225_000,
        z_tv = 0
      }
    }

pollShardV1Params :: V1PollShard.PollParams
pollShardV1Params =
  V1PollShard.PollParams
    { pollToken = Spooky.toSpookyAssetClass $ mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , stakingToken = Spooky.toSpookyAssetClass $ mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , indyAsset = Spooky.toSpookyAssetClass $ mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , stakingValHash = Spooky.toSpookyValidatorHash "54e8d424816f5bbd423353009ff7c31ada2cbdefe651175014117f46"
    }

executeV1Params :: V1Execute.ExecuteParams
executeV1Params =
  V1Execute.ExecuteParams
    { V1Execute.govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , V1Execute.upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , V1Execute.iAssetToken = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , V1Execute.stabilityPoolToken = mkAssetClass "3f28fb7d6c40468262dffb1c3adb568b342499826b664d940085d022" "STABILITY_POOL"
    , V1Execute.versionRecordToken = mkAssetClass "b92ce2123ad58c5cbec99ae4271c7d815c33e2845519af56b2423982" "VERSION_RECORD"
    , V1Execute.cdpValHash = "e4d2fb0b8d275852103fd75801e2c7dcf6ed3e276c74cabadbe5b8b6"
    , V1Execute.sPoolValHash = "a473cb8eb0b61c03b8696fceab1c1a89fa3ec834572850e7c2abe783"
    , V1Execute.versionRegistryValHash = "c3127796cc9658ac4fb3775c873a283be18ce33780a6817780f95fbe"
    }


-- |-----------------------------------------------------------------------
-- | V1 Validator Hashes
-- |-----------------------------------------------------------------------

versionRegistryV1Hash :: Ledger.ValidatorHash
versionRegistryV1Hash = validatorHash V1GovScript.versionRegistryScript

collectorV1Hash :: Ledger.ValidatorHash
collectorV1Hash = validatorHash $ V1CollectorScript.collectorScript collectorV1Params

treasuryV1Hash :: Ledger.ValidatorHash
treasuryV1Hash = validatorHash $ V1TreasuryScript.treasuryScript treasuryV1Params

stabilityPoolV1Hash :: Ledger.ValidatorHash
stabilityPoolV1Hash = validatorHash $ V1StabilityPoolScript.stabilityPoolScript stabilityPoolV1Params

cdpV1Hash :: Ledger.ValidatorHash
cdpV1Hash = validatorHash $ V1CDPScript.cdpScript cdpV1Params

cdpCreatorV1Hash :: Ledger.ValidatorHash
cdpCreatorV1Hash = validatorHash $ V1CDPScript.cdpCreatorScript cdpCreatorV1Params

stakingV1Hash :: Ledger.ValidatorHash
stakingV1Hash = validatorHash $ V1StakingScript.stakingScript stakingV1Params

pollManagerV1Hash :: Ledger.ValidatorHash
pollManagerV1Hash = validatorHash $ V1GovScript.pollManagerScript pollManagerV1Params

pollShardV1Hash :: Ledger.ValidatorHash
pollShardV1Hash = validatorHash $ V1GovScript.pollScript pollShardV1Params

govV1Hash :: Ledger.ValidatorHash
govV1Hash = validatorHash $ V1GovScript.govScript govV1Params

executeV1Hash :: Ledger.ValidatorHash
executeV1Hash = validatorHash $ V1GovScript.executeScript executeV1Params

versionRecordV1Symbol :: Value.CurrencySymbol
versionRecordV1Symbol = V1GovScript.versionRecordSymbol versionRecordV1Params

-- |-----------------------------------------------------------------------
-- | V2 Paramters
-- |-----------------------------------------------------------------------

versionRecordV2Params :: V2VersionRegistry.VersionRecordParams
versionRecordV2Params =
  V2VersionRegistry.VersionRecordParams
    { upgradeToken = (V1VersionRegistry.upgradeToken versionRecordV1Params)
    }

collectorV2Params :: V2Collector.CollectorScriptParams
collectorV2Params = 
  V2Collector.CollectorScriptParams
    { stakingManagerNFT = Spooky.unSpookyAssetClass (V1Collector.stakingManagerNFT collectorV1Params)
    , stakingToken = Spooky.unSpookyAssetClass (V1Collector.stakingToken collectorV1Params)
    , versionRecordToken = versionRecordTokenV2AssetClass
    }
  
treasuryV2Params :: V2Treasury.TreasuryScriptParams
treasuryV2Params = 
  V2Treasury.MkTreasuryScriptParams
    { upgradeToken = (V1Gov.upgradeToken govV1Params)
    , versionRecordToken = versionRecordTokenV2AssetClass
    , treasuryUtxosStakeCredential = Nothing
    }

stabilityPoolV2Params :: V2StabilityPool.StabilityPoolParams
stabilityPoolV2Params =
  V2StabilityPool.StabilityPoolParams 
    { assetSymbol = (V1StabilityPool.assetSymbol stabilityPoolV1Params)
    , stabilityPoolToken = (V1StabilityPool.stabilityPoolToken stabilityPoolV1Params)
    , snapshotEpochToScaleToSumToken = (V1StabilityPool.snapshotEpochToScaleToSumToken stabilityPoolV1Params)
    , accountToken = (V1StabilityPool.accountToken stabilityPoolV1Params)
    , cdpToken = (V1StabilityPool.cdpToken stabilityPoolV1Params)
    , iAssetAuthToken = (V1CDP.iAssetAuthToken cdpV1Params)
    , versionRecordToken = versionRecordTokenV2AssetClass
    , collectorValHash = collectorV2Hash
    , govNFT = (V1StabilityPool.govNFT stabilityPoolV1Params)
    , accountCreateFeeLovelaces = (V1StabilityPool.accountCreateFeeLovelaces stabilityPoolV1Params)
    , accountAdjustmentFeeLovelaces = (V1StabilityPool.accountAdjustmentFeeLovelaces stabilityPoolV1Params)
    , requestCollateralLovelaces = requestCollateralLovelacesV2
    }

cdpV2Params :: V2CDP.CDPScriptParams
cdpV2Params =
  V2CDP.CDPScriptParams 
    { cdpAuthToken = (V1CDP.cdpAuthToken cdpV1Params)
    , cdpAssetSymbol = (V1CDP.cdpAssetSymbol cdpV1Params)
    , iAssetAuthToken = (V1CDP.iAssetAuthToken cdpV1Params)
    , stabilityPoolAuthToken = (V1CDP.stabilityPoolAuthToken cdpV1Params)
    , cVersionRecordToken = versionRecordTokenV2AssetClass
    , upgradeToken = (V1CDP.upgradeToken cdpV1Params)
    , collectorValHash = collectorV2Hash
    , spValHash = stabilityPoolV2Hash
    , govNFT = (V1CDP.govNFT cdpV1Params)
    , cMinCollateralInLovelace = (V1CDP.cMinCollateralInLovelace cdpV1Params)
    , partialRedemptionExtraFeeLovelace = partialRedemptionExtraFeeLovelaceV2
    , cBiasTime = cdpBiasTimeV2
    , treasuryValHash = treasuryV2Hash
    }

cdpCreatorV2Params :: V2CDP.CDPCreatorScriptParams
cdpCreatorV2Params =
  V2CDP.CDPCreatorScriptParams 
    { cdpCreatorNft = (V1CDP.cdpCreatorNft cdpCreatorV1Params)
    , cdpAssetCs = (V1CDP.cdpAssetCs cdpCreatorV1Params)
    , cdpAuthTk = (V1CDP.cdpAuthTk cdpCreatorV1Params)
    , iAssetAuthTk = (V1CDP.iAssetAuthTk cdpCreatorV1Params)
    , versionRecordToken = versionRecordTokenV2AssetClass
    , cdpScriptHash = cdpV2Hash
    , collectorScriptHash = collectorV2Hash
    , minCollateralInLovelace = (V1CDP.minCollateralInLovelace cdpCreatorV1Params)
    , ccBiasTime = cdpBiasTimeV2
    }

stakingV2Params :: V2Staking.StakingParams
stakingV2Params = 
  V2Staking.StakingParams
    { stakingManagerNFT = (V1Staking.stakingManagerNFT stakingV1Params)
    , stakingToken = (V1Staking.stakingToken stakingV1Params)
    , indyToken = (V1Staking.indyToken stakingV1Params)
    , pollToken = (V1Staking.pollToken stakingV1Params)
    , versionRecordToken = versionRecordTokenV2AssetClass
    , collectorValHash = collectorV2Hash
    }

executeV2Params :: V2Execute.ExecuteParams
executeV2Params =
  V2Execute.ExecuteParams
    { V2Execute.govNFT = (V1Execute.govNFT executeV1Params)
    , V2Execute.upgradeToken = (V1Execute.upgradeToken executeV1Params)
    , V2Execute.iAssetToken = (V1Execute.iAssetToken executeV1Params)
    , V2Execute.stabilityPoolToken = (V1Execute.stabilityPoolToken executeV1Params)
    , V2Execute.versionRecordToken = versionRecordTokenV2AssetClass
    , V2Execute.cdpValHash = cdpV2Hash
    , V2Execute.sPoolValHash = stabilityPoolV2Hash
    , V2Execute.versionRegistryValHash = versionRegistryV2Hash
    , V2Execute.treasuryValHash = treasuryV2Hash
    , V2Execute.maxInterestPeriods = maxInterestPeriodsV2
    , V2Execute.indyToken = (V1Staking.indyToken stakingV1Params)
    }

pollManagerV2Params :: V2Poll.PollManagerParams
pollManagerV2Params =
  V2Poll.PollManagerParams
    { V2Poll.govNft = (V1PollManager.govNFT pollManagerV1Params)
    , V2Poll.pollToken = (V1PollManager.pollToken pollManagerV1Params)
    , V2Poll.upgradeToken = (V1PollManager.upgradeToken pollManagerV1Params)
    , V2Poll.indyAsset = (V1PollManager.indyAsset pollManagerV1Params)
    , V2Poll.govExecuteValHash = executeV2Hash
    , V2Poll.pBiasTime = (V1PollManager.pBiasTime pollManagerV1Params)
    , V2Poll.shardsValHash = pollShardV2Hash
    , V2Poll.treasuryValHash = treasuryV2Hash
    , V2Poll.initialIndyDistribution = (V1PollManager.initialIndyDistribution pollManagerV1Params)
    }

pollShardV2Params :: V2Poll.PollParams
pollShardV2Params =
  V2Poll.PollParams
    { pollToken = Spooky.unSpookyAssetClass (V1PollShard.pollToken pollShardV1Params)
    , stakingToken = Spooky.unSpookyAssetClass (V1PollShard.stakingToken pollShardV1Params)
    , indyAsset = Spooky.unSpookyAssetClass (V1PollShard.indyAsset pollShardV1Params)
    , stakingValHash = stakingV2Hash
    }

govV2Params :: V2Gov.GovParams
govV2Params =
  V2Gov.GovParams
    { govNFT = (V1Gov.govNFT govV1Params)
    , pollToken = (V1Gov.pollToken govV1Params)
    , upgradeToken = (V1Gov.upgradeToken govV1Params)
    , indyAsset = (V1Gov.indyAsset govV1Params)
    , versionRecordToken = versionRecordTokenV2AssetClass
    , pollManagerValHash = pollManagerV2Hash
    , gBiasTime = (V1Gov.gBiasTime govV1Params)
    , daoIdentityToken = daoIdentityTokenV2
    , iassetAuthToken = (V1CDP.iAssetAuthToken cdpV1Params)
    }

-- |-----------------------------------------------------------------------
-- | V2 Validator Hashes
-- |-----------------------------------------------------------------------

versionRecordTokenV2CurrencySymbol :: Value.CurrencySymbol
versionRecordTokenV2CurrencySymbol = V2GovScript.versionRecordSymbol versionRecordV2Params

versionRecordTokenV2AssetClass :: Value.AssetClass
versionRecordTokenV2AssetClass = Value.AssetClass (versionRecordTokenV2CurrencySymbol, "VERSION_RECORD")

versionRegistryV2Hash :: Ledger.ValidatorHash
versionRegistryV2Hash = validatorHash V2GovScript.versionRegistryScript

collectorV2Hash :: Ledger.ValidatorHash
collectorV2Hash = validatorHash $ V2CollectorScript.collectorScript collectorV2Params

treasuryV2Hash :: Ledger.ValidatorHash
treasuryV2Hash = validatorHash $ V2TreasuryScript.treasuryScript treasuryV2Params

stabilityPoolV2Hash :: Ledger.ValidatorHash
stabilityPoolV2Hash = validatorHash $ V2StabilityPoolScript.stabilityPoolScript stabilityPoolV2Params

cdpV2Hash :: Ledger.ValidatorHash
cdpV2Hash = validatorHash $ V2CDPScript.cdpScript cdpV2Params

cdpCreatorV2Hash :: Ledger.ValidatorHash
cdpCreatorV2Hash = validatorHash $ V2CDPScript.cdpCreatorScript cdpCreatorV2Params

stakingV2Hash :: Ledger.ValidatorHash
stakingV2Hash = validatorHash $ V2StakingScript.stakingScript stakingV2Params

pollManagerV2Hash :: Ledger.ValidatorHash
pollManagerV2Hash = validatorHash $ V2GovScript.pollManagerScript pollManagerV2Params

pollShardV2Hash :: Ledger.ValidatorHash
pollShardV2Hash = validatorHash $ V2GovScript.pollShardScript pollShardV2Params

govV2Hash :: Ledger.ValidatorHash
govV2Hash = validatorHash $ V2GovScript.govScript govV2Params

executeV2Hash :: Ledger.ValidatorHash
executeV2Hash = validatorHash $ V2GovScript.executeScript executeV2Params

-- |-----------------------------------------------------------------------
-- | Upgrade Minting Policies
-- |-----------------------------------------------------------------------

cdpCreatorUpgradeCurrencySymbol :: Value.CurrencySymbol
cdpCreatorUpgradeCurrencySymbol = UpgradesCDPScript.cdpCreatorUpgradeSymbol cdpCreatorV1Hash cdpCreatorV2Hash

cdpUpgradeCurrencySymbol :: Value.CurrencySymbol
cdpUpgradeCurrencySymbol = UpgradesCDPScript.cdpUpgradeSymbol cdpV1Hash cdpV2Hash

collectorUpgradeCurrencySymbol :: Value.CurrencySymbol
collectorUpgradeCurrencySymbol = UpgradesCollectorScript.collectorUpgradeSymbol collectorV1Hash collectorV2Hash

govUpgradeCurrencySymbol :: Value.CurrencySymbol
govUpgradeCurrencySymbol = UpgradesGovernanceScript.governanceUpgradeSymbol govV1Hash govV2Hash

stabilityPoolUpgradeCurrencySymbol :: Value.CurrencySymbol
stabilityPoolUpgradeCurrencySymbol = UpgradesStabilityPoolScript.stabilityPoolUpgradeSymbol stabilityPoolV1Hash stabilityPoolV2Hash

stakingUpgradeCurrencySymbol :: Value.CurrencySymbol
stakingUpgradeCurrencySymbol = UpgradesStakingScript.stakingUpgradeSymbol stakingV1Hash stakingV2Hash

treasuryUpgradeCurrencySymbol :: Value.CurrencySymbol
treasuryUpgradeCurrencySymbol = UpgradesTreasuryScript.treasuryUpgradeSymbol treasuryV1Hash treasuryV2Hash

-- |-----------------------------------------------------------------------
-- | Helper Functions
-- |-----------------------------------------------------------------------
mkCurrencySymbol :: BS.ByteString -> Value.CurrencySymbol
mkCurrencySymbol csName = do
  let Right csBS = decode csName
  Value.currencySymbol csBS

mkAssetClass :: BS.ByteString -> Value.TokenName -> Value.AssetClass
mkAssetClass csName tkName = Value.assetClass (mkCurrencySymbol csName) tkName

main :: IO ()
main = do
  putStrLn "---- V1 Validator Hashes ----"
  
  print $ "CDP Creator V1 Hash: " <> show cdpCreatorV1Hash
  print $ "CDP V1 Hash: " <> show cdpV1Hash
  print $ "Collector V1 Hash: " <> show collectorV1Hash
  print $ "Execute V1 Hash: " <> show executeV1Hash
  print $ "Governance V1 Hash: " <> show govV1Hash
  print $ "Poll Manager V1 Hash: " <> show pollManagerV1Hash
  print $ "Poll Shard V1 Hash: " <> show pollShardV1Hash
  print $ "Stability Pool V1 Hash: " <> show stabilityPoolV1Hash
  print $ "Staking V1 Hash: " <> show stakingV1Hash
  print $ "Treasury V1 Hash: " <> show treasuryV1Hash
  print $ "Version Record V1 Symbol: " <> show versionRecordV1Symbol
  print $ "Version Registry V1 Hash: " <> show versionRegistryV1Hash

  putStrLn "---- V2 Validator Hashes ----"

  print $ "CDP Creator V2 Hash: " <> show (cdpCreatorV2Hash)
  print $ "CDP V2 Hash: " <> show (cdpV2Hash)
  print $ "Collector V2 Hash: " <> show (collectorV2Hash)
  print $ "Execute V2 Hash: " <> show (executeV2Hash)
  print $ "Governance V2 Hash: " <> show (govV2Hash)
  print $ "Poll Manager V2 Hash: " <> show (pollManagerV2Hash)
  print $ "Poll Shard V2 Hash: " <> show (pollShardV2Hash)
  print $ "Stability Pool V2 Hash: " <> show (stabilityPoolV2Hash)
  print $ "Staking V2 Hash: " <> show (stakingV2Hash)
  print $ "Treasury V2 Hash: " <> show (treasuryV2Hash)
  print $ "Version Record V2 Symbol: " <> show versionRecordTokenV2CurrencySymbol
  print $ "Version Registry V2 Hash: " <> show (versionRegistryV2Hash)

  putStrLn "---- V2 Upgrade Symbols ----"

  print $ "CDP Creator Upgrade Symbol: " <> show cdpCreatorUpgradeCurrencySymbol
  print $ "CDP Upgrade Symbol: " <> show cdpUpgradeCurrencySymbol
  print $ "Collector Upgrade Symbol: " <> show collectorUpgradeCurrencySymbol
  print $ "Gov Upgrade Symbol: " <> show govUpgradeCurrencySymbol
  print $ "Stability Pool Upgrade Symbol: " <> show stabilityPoolUpgradeCurrencySymbol
  print $ "Staking Upgrade Symbol: " <> show stakingUpgradeCurrencySymbol
  print $ "Treasury Upgrade Symbol: " <> show treasuryUpgradeCurrencySymbol

