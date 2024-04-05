{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.V1.Lib.Treasury.Script
  ( treasuryUpgradePolicy,
    treasuryUpgradeSymbol,
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

data PolicyParams = PolicyParams
  { treasuryV1Hash :: V2.ValidatorHash,
    treasuryV2Hash :: V2.ValidatorHash,
    redeemerIdx :: Integer
  }

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

treasuryUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
treasuryUpgradePolicy treasuryV1Hash treasuryV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledTreasuryUpgradePolicy))
        [ V2.toData $
            PolicyParams
              { treasuryV1Hash = treasuryV1Hash,
                treasuryV2Hash = treasuryV2Hash,
                redeemerIdx = 0
              }
        ]

compiledTreasuryUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledTreasuryUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_keep_value_void_datum.named-debruijn")

treasuryUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
treasuryUpgradeSymbol v1 v2 = scriptCurrencySymbol $ treasuryUpgradePolicy v1 v2
