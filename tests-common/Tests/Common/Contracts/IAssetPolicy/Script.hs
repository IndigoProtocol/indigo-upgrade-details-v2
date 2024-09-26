{-# LANGUAGE TemplateHaskell #-}

module Tests.Common.Contracts.IAssetPolicy.Script
  ( iAssetPolicy,
    iAssetSymbol,
  )
where

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model.V2 (TypedPolicy, mkTypedPolicy', scriptCurrencySymbol)
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
  )
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

-- | NOTICE: IAsset policy needs to remain unchanged so for the tests we keep the IAsset policy
-- written in PlutusTx so it's the same as mainnet and the tests and mostly benchmarks are accurate.
iAssetPolicy :: Value.AssetClass -> TypedPolicy ()
iAssetPolicy params =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledIAssetPolicy))
        [V2.toData params]

compiledIAssetPolicy :: PlutusTx.CompiledCode UntypedMintingPolicy
compiledIAssetPolicy =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-common/data/iasset_policy.named-debruijn"))

iAssetSymbol :: Value.AssetClass -> Ledger.CurrencySymbol
iAssetSymbol = scriptCurrencySymbol . iAssetPolicy
