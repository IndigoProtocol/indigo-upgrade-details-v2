{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.VX.Lib.Governance.Script 
  ( GovernanceUpgradePolicyParams (..),
    governanceUpgradePolicy, 
    governanceUpgradeSymbol
  ) where

import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Plutus.Model (TypedPolicy)
import Plutus.Model.V2 (mkTypedPolicy', scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as P
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
import Tests.Common.THUtils (projectDirectory)

data GovernanceUpgradePolicyParams = GovernanceUpgradePolicyParams
  { govOldHash :: V2.ValidatorHash,
    govNewHash :: V2.ValidatorHash
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''GovernanceUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''GovernanceUpgradePolicyParams [('GovernanceUpgradePolicyParams, 0)]

governanceUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> TypedPolicy ()
governanceUpgradePolicy governanceOldHash governanceNewHash =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledGovernanceUpgradePolicy))
        [ V2.toData
            GovernanceUpgradePolicyParams
              { govOldHash = governanceOldHash,
                govNewHash = governanceNewHash
              }
        ]

compiledGovernanceUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledGovernanceUpgradePolicy =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-upgrades-lib/data/upgrade_govv2_to_govv21.named-debruijn"))

governanceUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  V2.CurrencySymbol
governanceUpgradeSymbol oldHash newHash = scriptCurrencySymbol $ governanceUpgradePolicy oldHash newHash
