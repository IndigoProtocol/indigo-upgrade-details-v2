{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.VX.Lib.StabilityPool.Script
  ( StabilityPoolUpgradePolicyParams(..),
    stabilityPoolUpgradePolicy,
    stabilityPoolUpgradeSymbol,
  )
where

import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Plutus.Model (TypedPolicy)
import Plutus.Model.V2 (mkTypedPolicy', scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as P
import Tests.Common.THUtils (projectDirectory)

data StabilityPoolUpgradePolicyParams = StabilityPoolUpgradePolicyParams
  { stabilityPoolOldHash :: V2.ValidatorHash,
    stabilityPoolNewHash :: V2.ValidatorHash
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''StabilityPoolUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''StabilityPoolUpgradePolicyParams [('StabilityPoolUpgradePolicyParams, 0)]

stabilityPoolUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
stabilityPoolUpgradePolicy spOldHash spNewHash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledStabilityPoolUpgradePolicy))
        [ V2.toData
            StabilityPoolUpgradePolicyParams
              { stabilityPoolOldHash = spOldHash,
                stabilityPoolNewHash = spNewHash
              }
        ]

compiledStabilityPoolUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledStabilityPoolUpgradePolicy =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-vX-upgrades-lib/data/upgrade_stabilitypoolv2_to_stabilitypoolv21.named-debruijn"))

stabilityPoolUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
stabilityPoolUpgradeSymbol old new = scriptCurrencySymbol $ stabilityPoolUpgradePolicy old new
