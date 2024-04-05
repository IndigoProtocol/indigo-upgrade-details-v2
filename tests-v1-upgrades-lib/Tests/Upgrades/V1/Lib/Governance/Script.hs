{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.V1.Lib.Governance.Script (governanceUpgradePolicy, governanceUpgradeSymbol) where

import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Plutus.Model (TypedPolicy)
import Plutus.Model.V2 (mkTypedPolicy', scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.LoadValidatorUtil (loadPolicyUsingEnvVar)

data GovernanceUpgradePolicyParams = GovernanceUpgradePolicyParams
  { govV1Hash :: V2.ValidatorHash,
    govV2Hash :: V2.ValidatorHash
  }

PlutusTx.makeLift ''GovernanceUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''GovernanceUpgradePolicyParams [('GovernanceUpgradePolicyParams, 0)]

governanceUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
governanceUpgradePolicy governanceV1Hash governanceV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledGovernanceUpgradePolicy))
        [ V2.toData
            GovernanceUpgradePolicyParams
              { govV1Hash = governanceV1Hash,
                govV2Hash = governanceV2Hash
              }
        ]

compiledGovernanceUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledGovernanceUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_govv1_to_govv2.named-debruijn")

governanceUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
governanceUpgradeSymbol v1 v2 = scriptCurrencySymbol $ governanceUpgradePolicy v1 v2
