module Main (main) where

import Data.ByteString.Base16
import qualified Data.ByteString as BS

import Cardano.Binary qualified as CBOR

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model.V2 (validatorHash)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as AssocMap

import Indigo.V2.Contracts.CDP.Common as V2CDP
import Indigo.V2.Contracts.Collector.Common as V2Collector
import Indigo.V2.Contracts.Governance.Gov.Common as V2Gov
import Indigo.V2.Contracts.Governance.Execute.Common as V2Execute
import Indigo.V2.Contracts.Governance.Poll.Common as V2PollManager (PollManagerParams (..)) 
import Indigo.V2.Contracts.Governance.Poll.Common as V2PollShard (PollParams (..)) 
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

import Indigo.VX.Contracts.CDP.Common as VXCDP
import Indigo.VX.Contracts.Collector.Common as VXCollector
import Indigo.VX.Contracts.Governance.Gov.Common as VXGov
import Indigo.VX.Contracts.Governance.Execute.Common as VXExecute
import Indigo.VX.Contracts.Governance.Poll.Common as VXPollManager (PollManagerParams (..))
import Indigo.VX.Contracts.Governance.Poll.Common as VXPollShard (PollParams (..)) 
import Indigo.VX.Contracts.StabilityPool.Common as VXStabilityPool
import Indigo.VX.Contracts.Staking.Common as VXStaking
import Indigo.VX.Contracts.Treasury.Common as VXTreasury

import Tests.VX.Lib.CDP.Script as VXCDPScript
import Tests.VX.Lib.Collector.Script as VXCollectorScript
import Tests.VX.Lib.Governance.Script as VXGovScript
import Tests.VX.Lib.StabilityPool.Script as VXStabilityPoolScript
import Tests.VX.Lib.Staking.Script as VXStakingScript
import Tests.VX.Lib.Treasury.Script as VXTreasuryScript

import Tests.Upgrades.VX.Lib.CDP.Script as UpgradesCDPScript
import Tests.Upgrades.VX.Lib.Governance.Script as UpgradesGovernanceScript
import Tests.Upgrades.VX.Lib.StabilityPool.Script as UpgradesStabilityPoolScript
import Tests.Upgrades.VX.Lib.Treasury.Script as UpgradesTreasuryScript

import Prelude

-- |-----------------------------------------------------------------------
-- | Constants
-- |-----------------------------------------------------------------------

-- This staking credential is the Indigo Foundation multi-sig staking credential.
-- https://cexplorer.io/stake/stake17xurtz4d6vxxp6apdpsg440gw4vjaxmuhrrspqnum6rlngcp8emwq
treasuryStakingCredential :: V2.StakingCredential
treasuryStakingCredential = do
  let Right scriptHash = decode "b8358aadd30c60eba168608ad5e875592e9b7cb8c700827cde87f9a3"
  V2.StakingHash $ V2.ScriptCredential $ V2.ValidatorHash $ V2.toBuiltin scriptHash

-- These match the oracles that have been published in mainnet.
-- Cross reference with this document: https://docs.google.com/document/d/1CTz40_7CV7cBiXIA_zrdk6NHDPA014l5aAZ4RuaFsjM/edit
assetInterestOracleMap :: AssocMap.Map V2.TokenName Value.AssetClass
assetInterestOracleMap = AssocMap.fromList [
    ("iBTC", (mkAssetClass "61dd539719ac5aa2ccc493789796f17039d632e0d0a0d11a43dd5cfe" "iBTC_INTEREST")),
    ("iETH", (mkAssetClass "7b75e317505dddce858ae7bf200656a967c7544e55efa5d18ef30249" "iETH_INTEREST")),
    ("iUSD", (mkAssetClass "eedb4a24cea6132d3dae1966217f86900e1d8c6a0d668408ecd7eb1b" "iUSD_INTEREST"))
  ]

-- |-----------------------------------------------------------------------
-- | V2 Paramters
-- |-----------------------------------------------------------------------
-- These values can be found in the mainnet system params: https://config.indigoprotocol.io/mainnet/mainnet-system-params-v2-ctl92.json

versionRecordV2Params :: V2VersionRegistry.VersionRecordParams
versionRecordV2Params =
  V2VersionRegistry.VersionRecordParams
    { upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    }

collectorV2Params :: V2Collector.CollectorScriptParams
collectorV2Params = 
  V2Collector.CollectorScriptParams
    { stakingManagerNFT = mkAssetClass "24b458412c2a7f9acb9c53c7ec4325b36806912ed56d2f91bfcf4d26" "STAKING_MANAGER_NFT"
    , stakingToken = mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    }
  
treasuryV2Params :: V2Treasury.TreasuryScriptParams
treasuryV2Params = 
  V2Treasury.MkTreasuryScriptParams
    { upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , treasuryUtxosStakeCredential = Nothing
    }

stabilityPoolV2Params :: V2StabilityPool.StabilityPoolParams
stabilityPoolV2Params =
  V2StabilityPool.StabilityPoolParams 
    { assetSymbol = mkCurrencySymbol "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880"
    , stabilityPoolToken = mkAssetClass "3f28fb7d6c40468262dffb1c3adb568b342499826b664d940085d022" "STABILITY_POOL"
    , snapshotEpochToScaleToSumToken = mkAssetClass "98ebc5df52729be30f9983cc878acba0a1cabbf2fd329b5a16214720" "SP_EPOCH"
    , accountToken = mkAssetClass "443c51db609bba8b2aa4c8af248bf797cbfcfa1e413c443296a50813" "SP_ACCOUNT"
    , cdpToken = mkAssetClass "708f5e6d597fc038d09a738d7be32edd6ea779d6feb32a53668d9050" "CDP"
    , iAssetAuthToken = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , collectorValHash = "0752abd65a0c983bfb1c9c3880cc632c099ba3adb2fe307afb4bbd9c"
    , govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , accountCreateFeeLovelaces = 5_000_000
    , accountAdjustmentFeeLovelaces = 1_000_000
    , requestCollateralLovelaces = 9_000_000
    }

cdpV2Params :: V2CDP.CDPScriptParams
cdpV2Params =
  V2CDP.CDPScriptParams 
    { cdpAuthToken = mkAssetClass "708f5e6d597fc038d09a738d7be32edd6ea779d6feb32a53668d9050" "CDP"
    , cdpAssetSymbol = mkCurrencySymbol "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880"
    , iAssetAuthToken = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , stabilityPoolAuthToken = mkAssetClass "3f28fb7d6c40468262dffb1c3adb568b342499826b664d940085d022" "STABILITY_POOL"
    , cVersionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , collectorValHash = "0752abd65a0c983bfb1c9c3880cc632c099ba3adb2fe307afb4bbd9c"
    , spValHash = "918765cae1147f5f2d914d45059f8b6497caa149ea94dd8da1f683ce"
    , govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , cMinCollateralInLovelace = 10_000_000
    , partialRedemptionExtraFeeLovelace = 10_000_000
    , cBiasTime = 1_200_000
    , treasuryValHash = "16c0a490c721920096645c5594499b2cb1d4067566a9e85855b9326c"
    }

cdpCreatorV2Params :: V2CDP.CDPCreatorScriptParams
cdpCreatorV2Params =
  V2CDP.CDPCreatorScriptParams 
    { cdpCreatorNft = mkAssetClass "735b37149eb0c2a5fb590bd60e39fe90ae3a96b6065b05d7aca99ebb" "CDP_CREATOR"
    , cdpAssetCs = mkCurrencySymbol "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880"
    , cdpAuthTk = mkAssetClass "708f5e6d597fc038d09a738d7be32edd6ea779d6feb32a53668d9050" "CDP"
    , iAssetAuthTk = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , cdpScriptHash = "0d8aee2011f769502cdc07273f7e118722ebf9c585f3fee061986b7b"
    , collectorScriptHash = "0752abd65a0c983bfb1c9c3880cc632c099ba3adb2fe307afb4bbd9c"
    , minCollateralInLovelace = 10_000_000
    , ccBiasTime = 1_200_000
    }

stakingV2Params :: V2Staking.StakingParams
stakingV2Params = 
  V2Staking.StakingParams
    { stakingManagerNFT = mkAssetClass "24b458412c2a7f9acb9c53c7ec4325b36806912ed56d2f91bfcf4d26" "STAKING_MANAGER_NFT"
    , stakingToken = mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , indyToken = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , collectorValHash = "0752abd65a0c983bfb1c9c3880cc632c099ba3adb2fe307afb4bbd9c"
    }

executeV2Params :: V2Execute.ExecuteParams
executeV2Params =
  V2Execute.ExecuteParams
    { V2Execute.govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , V2Execute.upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , V2Execute.iAssetToken = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    , V2Execute.stabilityPoolToken = mkAssetClass "3f28fb7d6c40468262dffb1c3adb568b342499826b664d940085d022" "STABILITY_POOL"
    , V2Execute.versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , V2Execute.cdpValHash = "0d8aee2011f769502cdc07273f7e118722ebf9c585f3fee061986b7b"
    , V2Execute.sPoolValHash = "918765cae1147f5f2d914d45059f8b6497caa149ea94dd8da1f683ce"
    , V2Execute.versionRegistryValHash = "ea84d625650d066e1645e3e81d9c70a73f9ed837bd96dc49850ae744"
    , V2Execute.treasuryValHash = "16c0a490c721920096645c5594499b2cb1d4067566a9e85855b9326c"
    , V2Execute.maxInterestPeriods = 40
    , V2Execute.indyToken = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    }

pollManagerV2Params :: V2PollManager.PollManagerParams
pollManagerV2Params =
  V2PollManager.PollManagerParams
    { V2PollManager.govNft = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , V2PollManager.pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , V2PollManager.upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , V2PollManager.indyAsset = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , V2PollManager.govExecuteValHash = "ac64deeeab59a038158d4b2e22801253e7a9a87a313479acb7ac46ed"
    , V2PollManager.pBiasTime = 1_200_000
    , V2PollManager.shardsValHash = "8110c6ae9c92962f01ce98611656c05ea69aa5a709f4ea89c588bbdb"
    , V2PollManager.treasuryValHash = "16c0a490c721920096645c5594499b2cb1d4067566a9e85855b9326c"
    , V2PollManager.initialIndyDistribution = 1_575_000_000_000
    }

pollShardV2Params :: V2PollShard.PollParams
pollShardV2Params =
  V2PollShard.PollParams
    { V2PollShard.pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , V2PollShard.stakingToken = mkAssetClass "fd0d72fafee1d230a74c31ac503a192abd5b71888ae3f94128c1e634" "STAKING_POSITION"
    , V2PollShard.indyAsset = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , V2PollShard.stakingValHash = "a23793f529179e09cefb3c37fc6ae081e0e99e99be5cdb55a00941a5"
    }

govV2Params :: V2Gov.GovParams
govV2Params =
  V2Gov.GovParams
    { V2Gov.govNFT = mkAssetClass "2fccae8bc1c8553a2185b2e77ccdea22f2e1d6e87beb80ef4eaf8cce" "GOV_NFT"
    , V2Gov.pollToken = mkAssetClass "f9b162ea9529e639a083595294006a833473883a75d6df1e4c22dd4f" "POLL_MANAGER"
    , V2Gov.upgradeToken = mkAssetClass "ca72f111cc130ac311259181e0720516c044cee30704706a0299c2a8" "UPGRADE"
    , V2Gov.indyAsset = mkAssetClass "533bb94a8850ee3ccbe483106489399112b74c905342cb1792a797a0" "INDY"
    , V2Gov.versionRecordToken = mkAssetClass "d626ddf398b0bca6e112cf0b78c8124b989a6ca4e7c0dfe8c18c7c2e" "VERSION_RECORD"
    , V2Gov.pollManagerValHash = "31aaab10e57345a2ed5c37ca32c57b2d9dc6219d23b51c85919febe2"
    , V2Gov.gBiasTime = 1_200_000
    , V2Gov.daoIdentityToken = mkAssetClass "07d3770b2ddfa1cb35d32fab2446731f50efd9229ae4b1180552558d" "DAO"
    , V2Gov.iassetAuthToken = mkAssetClass "97da12de04a6b527cc3b3469c5e5485cf258dfd1021f12e728f2e714" "IASSET"
    }

-- |-----------------------------------------------------------------------
-- | V2 Validator Hashes
-- |-----------------------------------------------------------------------

versionRecordTokenV2CurrencySymbol :: Value.CurrencySymbol
versionRecordTokenV2CurrencySymbol = V2GovScript.versionRecordSymbol versionRecordV2Params

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
-- | V2.1 Parameters
-- |-----------------------------------------------------------------------
-- These should be carefully validated and reviewed to make sure no mistakes are made.

-- Essentially copy-pasting the V2 parameters.
collectorVXParams :: VXCollector.CollectorScriptParams
collectorVXParams = 
  VXCollector.CollectorScriptParams
    { stakingManagerNFT = V2Collector.stakingManagerNFT collectorV2Params
    , stakingToken = V2Collector.stakingToken collectorV2Params
    , versionRecordToken = V2Collector.versionRecordToken collectorV2Params
    }
  
-- The only difference between the V2 and VX treasury is the staking credential.
treasuryVXParams :: VXTreasury.TreasuryScriptParams
treasuryVXParams = 
  VXTreasury.MkTreasuryScriptParams
    { upgradeToken = V2Treasury.upgradeToken treasuryV2Params
    , versionRecordToken = V2Treasury.versionRecordToken treasuryV2Params
    , treasuryUtxosStakeCredential = Just treasuryStakingCredential
    }

-- Essentially a copy/paste of the V2 stability pool parameters.
stabilityPoolVXParams :: VXStabilityPool.StabilityPoolParams
stabilityPoolVXParams =
  VXStabilityPool.StabilityPoolParams 
    { assetSymbol = (V2StabilityPool.assetSymbol stabilityPoolV2Params)
    , stabilityPoolToken = (V2StabilityPool.stabilityPoolToken stabilityPoolV2Params)
    , snapshotEpochToScaleToSumToken = (V2StabilityPool.snapshotEpochToScaleToSumToken stabilityPoolV2Params)
    , accountToken = (V2StabilityPool.accountToken stabilityPoolV2Params)
    , cdpToken = (V2StabilityPool.cdpToken stabilityPoolV2Params)
    , iAssetAuthToken = (V2StabilityPool.iAssetAuthToken stabilityPoolV2Params)
    , versionRecordToken = (V2StabilityPool.versionRecordToken stabilityPoolV2Params)
    , collectorValHash = (V2StabilityPool.collectorValHash stabilityPoolV2Params)
    , govNFT = (V2StabilityPool.govNFT stabilityPoolV2Params)
    , accountCreateFeeLovelaces = (V2StabilityPool.accountCreateFeeLovelaces stabilityPoolV2Params)
    , accountAdjustmentFeeLovelaces = (V2StabilityPool.accountAdjustmentFeeLovelaces stabilityPoolV2Params)
    , requestCollateralLovelaces = (V2StabilityPool.requestCollateralLovelaces stabilityPoolV2Params)
    }

-- The only difference between the V2 and VX cdp parameters is the stability pool hash and treasury hash.
cdpVXParams :: VXCDP.CDPScriptParams
cdpVXParams =
  VXCDP.CDPScriptParams 
    { cdpAuthToken = (V2CDP.cdpAuthToken cdpV2Params)
    , cdpAssetSymbol = (V2CDP.cdpAssetSymbol cdpV2Params)
    , iAssetAuthToken = (V2CDP.iAssetAuthToken cdpV2Params)
    , stabilityPoolAuthToken = (V2CDP.stabilityPoolAuthToken cdpV2Params)
    , cVersionRecordToken = (V2CDP.cVersionRecordToken cdpV2Params)
    , upgradeToken = (V2CDP.upgradeToken cdpV2Params)
    , collectorValHash = (V2CDP.collectorValHash cdpV2Params)
    , spValHash = stabilityPoolVXHash -- We must use the new stability pool hash, due to a change in the iAsset daum
    , govNFT = (V2CDP.govNFT cdpV2Params)
    , cMinCollateralInLovelace = (V2CDP.cMinCollateralInLovelace cdpV2Params)
    , partialRedemptionExtraFeeLovelace = (V2CDP.partialRedemptionExtraFeeLovelace cdpV2Params)
    , cBiasTime = (V2CDP.cBiasTime cdpV2Params)
    , treasuryValHash = treasuryVXHash
    }

-- The only difference between the V2 and VX cdp creator parameters is the new CDP hash.
cdpCreatorVXParams :: VXCDP.CDPCreatorScriptParams
cdpCreatorVXParams =
  VXCDP.CDPCreatorScriptParams 
    { cdpCreatorNft = (V2CDP.cdpCreatorNft cdpCreatorV2Params)
    , cdpAssetCs = (V2CDP.cdpAssetCs cdpCreatorV2Params)
    , cdpAuthTk = (V2CDP.cdpAuthTk cdpCreatorV2Params)
    , iAssetAuthTk = (V2CDP.iAssetAuthTk cdpCreatorV2Params)
    , versionRecordToken = (V2CDP.versionRecordToken cdpCreatorV2Params)
    , cdpScriptHash = cdpVXHash
    , collectorScriptHash = (V2CDP.collectorScriptHash cdpCreatorV2Params)
    , minCollateralInLovelace = (V2CDP.minCollateralInLovelace cdpCreatorV2Params)
    , ccBiasTime = (V2CDP.ccBiasTime cdpCreatorV2Params)
    }

-- Essentially a copy/paste of the V2 staking parameters.
stakingVXParams :: VXStaking.StakingParams
stakingVXParams = 
  VXStaking.StakingParams
    { stakingManagerNFT = (V2Staking.stakingManagerNFT stakingV2Params)
    , stakingToken = (V2Staking.stakingToken stakingV2Params)
    , indyToken = (V2Staking.indyToken stakingV2Params)
    , pollToken = (V2Staking.pollToken stakingV2Params)
    , versionRecordToken = (V2Staking.versionRecordToken stakingV2Params)
    , collectorValHash = (V2Staking.collectorValHash stakingV2Params)
    }

-- Dependent on the new CDP, Stability Pool, and Treasury hashes.
executeVXParams :: VXExecute.ExecuteParams
executeVXParams =
  VXExecute.ExecuteParams
    { VXExecute.govNFT = (V2Execute.govNFT executeV2Params)
    , VXExecute.upgradeToken = (V2Execute.upgradeToken executeV2Params)
    , VXExecute.iAssetToken = (V2Execute.iAssetToken executeV2Params)
    , VXExecute.stabilityPoolToken = (V2Execute.stabilityPoolToken executeV2Params)
    , VXExecute.versionRecordToken = (V2Execute.versionRecordToken executeV2Params)
    , VXExecute.cdpValHash = cdpVXHash
    , VXExecute.sPoolValHash = stabilityPoolVXHash
    , VXExecute.versionRegistryValHash = (V2Execute.versionRegistryValHash executeV2Params)
    , VXExecute.treasuryValHash = treasuryVXHash
    , VXExecute.indyToken = (V2Execute.indyToken executeV2Params)
    }

-- Dependent on the new Execute, Poll Shard, and Treasury hashes.
pollManagerVXParams :: VXPollManager.PollManagerParams
pollManagerVXParams =
  VXPollManager.PollManagerParams
    { VXPollManager.govNft = (V2PollManager.govNft pollManagerV2Params)
    , VXPollManager.pollToken = (V2PollManager.pollToken pollManagerV2Params)
    , VXPollManager.upgradeToken = (V2PollManager.upgradeToken pollManagerV2Params)
    , VXPollManager.indyAsset = (V2PollManager.indyAsset pollManagerV2Params)
    , VXPollManager.govExecuteValHash = executeVXHash
    , VXPollManager.pBiasTime = (V2PollManager.pBiasTime pollManagerV2Params)
    , VXPollManager.shardsValHash = pollShardVXHash
    , VXPollManager.treasuryValHash = treasuryVXHash
    , VXPollManager.initialIndyDistribution = (V2PollManager.initialIndyDistribution pollManagerV2Params)
    }

-- Essentially a copy/paste of the V2 poll shard parameters.
pollShardVXParams :: VXPollShard.PollParams
pollShardVXParams =
  VXPollShard.PollParams
    { VXPollShard.pollToken = V2PollShard.pollToken pollShardV2Params
    , VXPollShard.stakingToken = V2PollShard.stakingToken pollShardV2Params
    , VXPollShard.indyAsset = V2PollShard.indyAsset pollShardV2Params
    , VXPollShard.stakingValHash = V2PollShard.stakingValHash pollShardV2Params
    }

-- Dependent on a new poll manager hash for V2.1
govVXParams :: VXGov.GovParams
govVXParams =
  VXGov.GovParams
    { VXGov.govNFT = (V2Gov.govNFT govV2Params)
    , VXGov.pollToken = (V2Gov.pollToken govV2Params)
    , VXGov.upgradeToken = (V2Gov.upgradeToken govV2Params)
    , VXGov.indyAsset = (V2Gov.indyAsset govV2Params)
    , VXGov.versionRecordToken = (V2Gov.versionRecordToken govV2Params)
    , VXGov.pollManagerValHash = pollManagerVXHash
    , VXGov.gBiasTime = (V2Gov.gBiasTime govV2Params)
    , VXGov.daoIdentityToken = (V2Gov.daoIdentityToken govV2Params)
    , VXGov.iassetAuthToken = (V2Gov.iassetAuthToken govV2Params)
    }

-- |-----------------------------------------------------------------------
-- | VX Validator Hashes
-- |-----------------------------------------------------------------------

collectorVXHash :: Ledger.ValidatorHash
collectorVXHash = validatorHash $ VXCollectorScript.collectorScript collectorVXParams

treasuryVXHash :: Ledger.ValidatorHash
treasuryVXHash = validatorHash $ VXTreasuryScript.treasuryScript treasuryVXParams

stabilityPoolVXHash :: Ledger.ValidatorHash
stabilityPoolVXHash = validatorHash $ VXStabilityPoolScript.stabilityPoolScript stabilityPoolVXParams

cdpVXHash :: Ledger.ValidatorHash
cdpVXHash = validatorHash $ VXCDPScript.cdpScript cdpVXParams

cdpCreatorVXHash :: Ledger.ValidatorHash
cdpCreatorVXHash = validatorHash $ VXCDPScript.cdpCreatorScript cdpCreatorVXParams

stakingVXHash :: Ledger.ValidatorHash
stakingVXHash = validatorHash $ VXStakingScript.stakingScript stakingVXParams

pollManagerVXHash :: Ledger.ValidatorHash
pollManagerVXHash = validatorHash $ VXGovScript.pollManagerScript pollManagerVXParams

pollShardVXHash :: Ledger.ValidatorHash
pollShardVXHash = validatorHash $ VXGovScript.pollShardScript pollShardVXParams

govVXHash :: Ledger.ValidatorHash
govVXHash = validatorHash $ VXGovScript.govScript govVXParams

executeVXHash :: Ledger.ValidatorHash
executeVXHash = validatorHash $ VXGovScript.executeScript executeVXParams

-- |-----------------------------------------------------------------------
-- | Upgrade Minting Policies
-- |-----------------------------------------------------------------------

cdpCreatorUpgradeCurrencySymbol :: Value.CurrencySymbol
cdpCreatorUpgradeCurrencySymbol = UpgradesCDPScript.cdpCreatorUpgradeSymbol cdpCreatorV2Hash cdpCreatorVXHash

cdpUpgradeCurrencySymbol :: Value.CurrencySymbol
cdpUpgradeCurrencySymbol = UpgradesCDPScript.cdpUpgradeSymbol cdpV2Hash cdpVXHash assetInterestOracleMap

govUpgradeCurrencySymbol :: Value.CurrencySymbol
govUpgradeCurrencySymbol = UpgradesGovernanceScript.governanceUpgradeSymbol govV2Hash govVXHash

stabilityPoolUpgradeCurrencySymbol :: Value.CurrencySymbol
stabilityPoolUpgradeCurrencySymbol = UpgradesStabilityPoolScript.stabilityPoolUpgradeSymbol stabilityPoolV2Hash stabilityPoolVXHash

treasuryUpgradeCurrencySymbol :: Value.CurrencySymbol
treasuryUpgradeCurrencySymbol = UpgradesTreasuryScript.treasuryUpgradeSymbol treasuryV2Hash treasuryVXHash (Just treasuryStakingCredential)

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
  -- Print out the hashes and parameters for the V2 contracts.
  -- These can be validated that they match the mainnet hashes by checking against a chain explorer.
  -- Additionally, the validator hashes are saved in the system params deployed here: https://config.indigoprotocol.io/mainnet/mainnet-system-params-v2-ctl92.json
  -- These hashes are then used to generate the upgrade policies for the V2.1 contracts.
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

  -- Next, we generate the v2.1 validator hashes.
  -- These can be validated by checking the VX parameters, making sure the appropriate 
  -- values are copied from the V2 contracts and any changed values are updated appropriately.
  -- In this case, the only validator that shouldn't have changed is the Staking and Collector validator.
  -- The Execute, Poll Manager, and Poll Shared are regenerated but don't require any upgrade.
  putStrLn "---- V2.1 Validator Hashes ----"

  print $ "CDP Creator VX Hash: " <> show (cdpCreatorVXHash)
  print $ "CDP VX Hash: " <> show (cdpVXHash)
  -- The collector validator should match the V2 validator hash.
  print $ "Collector VX Hash: " <> show (collectorVXHash)
  print $ "Execute VX Hash: " <> show (executeVXHash)
  print $ "Governance VX Hash: " <> show (govVXHash)
  print $ "Poll Manager VX Hash: " <> show (pollManagerVXHash)
  print $ "Poll Shard VX Hash: " <> show (pollShardVXHash)
  print $ "Stability Pool VX Hash: " <> show (stabilityPoolVXHash)
  -- The staking validator should match the V2 validator hash.
  print $ "Staking VX Hash: " <> show (stakingVXHash)
  print $ "Treasury VX Hash: " <> show (treasuryVXHash)

  -- Next, we generate the upgrade symbols for the V2.1 contracts.
  -- These can be validated by checking the upgrade policies and making sure the currency symbols match.
  -- Most of the upgrade policies are parameterized by their old and new validator hashes, 
  -- however the CDP upgrade adds the interest rate oracle map and the treasury upgrade adds the staking credential.
  -- It is crucial to validate that the interest rate oracle map is correctly serialized, as well as the staking credential.
  putStrLn "---- V2.1 Upgrade Symbols ----"

  print $ "CDP Creator Upgrade Symbol: " <> show cdpCreatorUpgradeCurrencySymbol
  print $ "CDP Upgrade Symbol: " <> show cdpUpgradeCurrencySymbol
  print $ "Gov Upgrade Symbol: " <> show govUpgradeCurrencySymbol
  print $ "Stability Pool Upgrade Symbol: " <> show stabilityPoolUpgradeCurrencySymbol
  print $ "Treasury Upgrade Symbol: " <> show treasuryUpgradeCurrencySymbol

  -- Print out the CBOR representation of the V2.1 parameters.
  -- This is meant for validation of the interest rate oracle map (CDP) and staking credential (Treasury)
  putStrLn "---- V2.1 Parameters ----"
  print $ "CDP Creator Params: " <> show cdpCreatorVXParams
  print $ "CDP Creator Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ cdpCreatorVXParams)
  print $ "Treasury Params: " <> show treasuryVXParams
  print $ "Treasury Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ treasuryVXParams)

  -- This should print out the CBOR representation of the V2.1 upgrade parameters.
  -- We can compare this to the aiken code base to make sure that aiken serializes the same way.
  -- For example, in validators/upgrade_cdpcreatorv2_to_cdpcreatorv21.ak (upgrade_cdp_creator_params_cbor function) 
  -- we should be able to take the output of "CDP Creator Upgrade Params CBOR" and get matching values.
  putStrLn "---- V2.1 Upgrade Parameters ----"
  let 
    cdpCreatorUpgradeParams = UpgradesCDPScript.CdpCreatorUpgradePolicyParams { creatorOldHash = cdpCreatorV2Hash, creatorNewHash = cdpCreatorVXHash }
    cdpUpgradeParams = UpgradesCDPScript.CDPUpgradePolicyParams { cdpOldHash = cdpV2Hash, cdpNewHash = cdpVXHash, interestRateOracles = assetInterestOracleMap }
    govUpgradeParams = UpgradesGovernanceScript.GovernanceUpgradePolicyParams { govOldHash = govV2Hash, govNewHash = govVXHash }
    stabilityPoolUpgradeParams = UpgradesStabilityPoolScript.StabilityPoolUpgradePolicyParams { stabilityPoolOldHash = stabilityPoolV2Hash, stabilityPoolNewHash = stabilityPoolVXHash }
    treasuryUpgradeParams = UpgradesTreasuryScript.TreasuryUpgradePolicyParams { treasuryOldHash = treasuryV2Hash, treasuryNewHash = treasuryVXHash, stakeCredential = Just treasuryStakingCredential }
  
  print $ "CDP Creator Upgrade Params: " <> show cdpCreatorUpgradeParams
  print $ "CDP Creator Upgrade Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ cdpCreatorUpgradeParams)
  print $ "CDP Upgrade Params: " <> show cdpUpgradeParams
  print $ "CDP Upgrade Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ cdpUpgradeParams)
  print $ "Gov Upgrade Params: " <> show govUpgradeParams
  print $ "Gov Upgrade Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ govUpgradeParams)
  print $ "Stability Pool Upgrade Params: " <> show stabilityPoolUpgradeParams
  print $ "Stability Pool Upgrade Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ stabilityPoolUpgradeParams)
  print $ "Treasury Upgrade Params: " <> show treasuryUpgradeParams
  print $ "Treasury Upgrade Params CBOR: " <> show (encode $ CBOR.serialize' $ V2.toData $ treasuryUpgradeParams)


