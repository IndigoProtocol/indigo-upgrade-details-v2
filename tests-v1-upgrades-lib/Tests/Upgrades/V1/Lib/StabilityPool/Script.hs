{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.V1.Lib.StabilityPool.Script
  ( stabilityPoolUpgradePolicy,
    stabilityPoolUpgradeSymbol,
  )
where

import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Plutus.Model (TypedPolicy)
import Plutus.Model.V2 (mkTypedPolicy', scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.LoadValidatorUtil (loadPolicyUsingEnvVar)

data StabilityPoolUpgradePolicyParams = StabilityPoolUpgradePolicyParams
  { stabilityPoolV1Hash :: V2.ValidatorHash,
    stabilityPoolV2Hash :: V2.ValidatorHash
  }

PlutusTx.makeLift ''StabilityPoolUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''StabilityPoolUpgradePolicyParams [('StabilityPoolUpgradePolicyParams, 0)]

stabilityPoolUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
stabilityPoolUpgradePolicy spV1Hash spV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledStabilityPoolUpgradePolicy))
        [ V2.toData
            StabilityPoolUpgradePolicyParams
              { stabilityPoolV1Hash = spV1Hash,
                stabilityPoolV2Hash = spV2Hash
              }
        ]

compiledStabilityPoolUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledStabilityPoolUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_stabilitypoolv1_to_stabilitypoolv2.named-debruijn")

stabilityPoolUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
stabilityPoolUpgradeSymbol v1 v2 = scriptCurrencySymbol $ stabilityPoolUpgradePolicy v1 v2
