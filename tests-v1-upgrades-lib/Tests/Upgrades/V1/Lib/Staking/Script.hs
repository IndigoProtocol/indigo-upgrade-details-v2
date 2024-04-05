{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.V1.Lib.Staking.Script
  ( stakingUpgradePolicy,
    stakingUpgradeSymbol,
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
import Tests.Common.LoadValidatorUtil (loadPolicyUsingEnvVar)

data StakingUpgradePolicyParams = StakingUpgradePolicyParams
  { stakingV1Hash :: V2.ValidatorHash,
    stakingV2Hash :: V2.ValidatorHash
  }
  deriving (Generic)

PlutusTx.makeLift ''StakingUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''StakingUpgradePolicyParams [('StakingUpgradePolicyParams, 0)]

stakingUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
stakingUpgradePolicy stakingV1Hash stakingV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledStakingUpgradePolicy))
        [ V2.toData
            StakingUpgradePolicyParams
              { stakingV1Hash,
                stakingV2Hash
              }
        ]

compiledStakingUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledStakingUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_stakingv1_to_stakingv2.named-debruijn")

stakingUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
stakingUpgradeSymbol v1 v2 = scriptCurrencySymbol $ stakingUpgradePolicy v1 v2
