{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.V1.Lib.Collector.Script (collectorUpgradePolicy, collectorUpgradeSymbol) where

import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Plutus.Model (TypedPolicy)
import Plutus.Model.V2 (mkTypedPolicy', scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.LoadValidatorUtil (loadPolicyUsingEnvVar)

data PolicyParams = PolicyParams
  { collectorV1Hash :: V2.ValidatorHash,
    collectorV2Hash :: V2.ValidatorHash,
    redeemerIdx :: Integer
  }

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

collectorUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
collectorUpgradePolicy collectorV1Hash collectorV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledCollectorUpgradePolicy))
        [ V2.toData $
            PolicyParams
              { collectorV1Hash = collectorV1Hash,
                collectorV2Hash = collectorV2Hash,
                redeemerIdx = 1
              }
        ]

compiledCollectorUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledCollectorUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_keep_value_void_datum.named-debruijn")

collectorUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
collectorUpgradeSymbol v1 v2 = scriptCurrencySymbol $ collectorUpgradePolicy v1 v2
