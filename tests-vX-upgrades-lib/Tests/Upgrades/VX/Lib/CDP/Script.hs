{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.VX.Lib.CDP.Script
  ( CdpCreatorUpgradePolicyParams (..),
    CDPUpgradePolicyParams (..),
    cdpCreatorUpgradePolicy,
    cdpCreatorUpgradeSymbol,
    cdpUpgradePolicy,
    cdpUpgradeSymbol,
  )
where

import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Ledger.Value qualified as Value
import Plutus.Model (TypedPolicy)
import Plutus.Model.V2 (mkTypedPolicy', scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude
import Prelude qualified as P
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
import Tests.Common.THUtils (projectDirectory)
import Tests.Upgrades.VX.Lib.CDP.Types (CDPUpgradeRedeemer)

data CdpCreatorUpgradePolicyParams = CdpCreatorUpgradePolicyParams
  { creatorOldHash :: V2.ValidatorHash,
    creatorNewHash :: V2.ValidatorHash
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''CdpCreatorUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''CdpCreatorUpgradePolicyParams [('CdpCreatorUpgradePolicyParams, 0)]

data CDPUpgradePolicyParams = CDPUpgradePolicyParams
  { cdpOldHash :: V2.ValidatorHash,
    cdpNewHash :: V2.ValidatorHash,
    interestRateOracles :: AssocMap.Map V2.TokenName Value.AssetClass
  }
  deriving (Generic, P.Show, P.Eq)

PlutusTx.makeLift ''CDPUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''CDPUpgradePolicyParams [('CDPUpgradePolicyParams, 0)]

cdpCreatorUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
cdpCreatorUpgradePolicy creatorOldHash creatorNewHash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledCDPCreatorUpgradePolicy))
        [ V2.toData
            CdpCreatorUpgradePolicyParams
              { creatorOldHash,
                creatorNewHash
              }
        ]

compiledCDPCreatorUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledCDPCreatorUpgradePolicy =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-upgrades-lib/data/upgrade_cdpcreatorv2_to_cdpcreatorv21.named-debruijn"))

cdpCreatorUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
cdpCreatorUpgradeSymbol new old = scriptCurrencySymbol $ cdpCreatorUpgradePolicy new old

cdpUpgradePolicy ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  AssocMap.Map V2.TokenName Value.AssetClass ->
  TypedPolicy CDPUpgradeRedeemer
cdpUpgradePolicy cdpOldHash cdpNewHash interestRateOracles =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledCDPUpgradePolicy))
        [ V2.toData
            CDPUpgradePolicyParams
              { cdpOldHash,
                cdpNewHash,
                interestRateOracles
              }
        ]

compiledCDPUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledCDPUpgradePolicy =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-upgrades-lib/data/upgrade_cdpv2_to_cdpv21.named-debruijn"))

cdpUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  AssocMap.Map V2.TokenName Value.AssetClass ->
  V2.CurrencySymbol
cdpUpgradeSymbol cdpOldHash cdpNewHash interestRateOracles = scriptCurrencySymbol $ cdpUpgradePolicy cdpOldHash cdpNewHash interestRateOracles
