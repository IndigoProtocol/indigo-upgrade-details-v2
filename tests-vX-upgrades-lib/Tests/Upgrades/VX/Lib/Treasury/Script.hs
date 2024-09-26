{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Upgrades.VX.Lib.Treasury.Script
  ( TreasuryUpgradePolicyParams (..),
    treasuryUpgradePolicy,
    treasuryUpgradeSymbol,
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
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
import Tests.Common.THUtils (projectDirectory)

data TreasuryUpgradePolicyParams = TreasuryUpgradePolicyParams
  { treasuryOldHash :: V2.ValidatorHash,
    treasuryNewHash :: V2.ValidatorHash,
    stakeCredential :: Maybe V2.StakingCredential
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''TreasuryUpgradePolicyParams
PlutusTx.makeIsDataIndexed ''TreasuryUpgradePolicyParams [('TreasuryUpgradePolicyParams, 0)]

treasuryUpgradePolicy :: V2.ValidatorHash -> V2.ValidatorHash -> Maybe V2.StakingCredential -> TypedPolicy ()
treasuryUpgradePolicy treasuryOldHash treasuryNewHash sc =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledTreasuryUpgradePolicy))
        [ V2.toData $
            TreasuryUpgradePolicyParams
              { treasuryOldHash = treasuryOldHash,
                treasuryNewHash = treasuryNewHash,
                stakeCredential = sc
              }
        ]

compiledTreasuryUpgradePolicy ::
  PlutusTx.CompiledCode UntypedMintingPolicy
compiledTreasuryUpgradePolicy =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-upgrades-lib/data/upgrade_treasuryv2_to_treasuryv21.named-debruijn"))

treasuryUpgradeSymbol ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  Maybe V2.StakingCredential ->
  V2.CurrencySymbol
treasuryUpgradeSymbol oldHash newHash sc = scriptCurrencySymbol $ treasuryUpgradePolicy oldHash newHash sc
