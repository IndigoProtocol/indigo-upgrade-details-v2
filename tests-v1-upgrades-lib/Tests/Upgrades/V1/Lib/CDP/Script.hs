{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.V1.Lib.CDP.Script
  ( cdpCreatorUpgradePolicy,
    cdpCreatorUpgradeSymbol,
    cdpUpgradePolicy,
    cdpUpgradeSymbol,
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

data BaseUpgradePolicyParams = BaseUpgradePolicyParams
  { creatorV1Hash :: V2.ValidatorHash,
    creatorV2Hash :: V2.ValidatorHash,
    redeemerConstructor :: Integer
  }
  deriving (Generic)

PlutusTx.makeLift ''BaseUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''BaseUpgradePolicyParams [('BaseUpgradePolicyParams, 0)]

data CDPUpgradePolicyParams = CDPUpgradePolicyParams
  { cdpV1Hash :: V2.ValidatorHash,
    cdpV2Hash :: V2.ValidatorHash
  }
  deriving (Generic)

PlutusTx.makeLift ''CDPUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''CDPUpgradePolicyParams [('CDPUpgradePolicyParams, 0)]

cdpCreatorUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
cdpCreatorUpgradePolicy creatorV1Hash creatorV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledCDPCreatorUpgradePolicy))
        [ V2.toData
            BaseUpgradePolicyParams
              { creatorV1Hash,
                creatorV2Hash,
                redeemerConstructor = 1
              }
        ]

compiledCDPCreatorUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledCDPCreatorUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_keep_value_void_datum.named-debruijn")

cdpCreatorUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
cdpCreatorUpgradeSymbol v1 v2 = scriptCurrencySymbol $ cdpCreatorUpgradePolicy v1 v2

cdpUpgradePolicy ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  TypedPolicy ()
cdpUpgradePolicy cdpV1Hash cdpV2Hash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledCDPUpgradePolicy))
        [ V2.toData
            CDPUpgradePolicyParams
              { cdpV1Hash,
                cdpV2Hash
              }
        ]

compiledCDPUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledCDPUpgradePolicy =
  $$(loadPolicyUsingEnvVar "upgrade_cdpv1_to_cdpv2.named-debruijn")

cdpUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
cdpUpgradeSymbol v1 v2 = scriptCurrencySymbol $ cdpUpgradePolicy v1 v2
