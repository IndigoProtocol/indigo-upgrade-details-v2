{-# LANGUAGE TemplateHaskell #-}

module Tests.Common.Contracts.AuthPolicy.Script (authPolicy, authPolicySymbol) where

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

-- | NOTICE: Some auth tokens need to remain unchanged so for the tests we keep the Auth policy
-- written in PlutusTx so it's the same as mainnet and the tests and mostly benchmarks are accurate.
authPolicy ::
  Value.AssetClass -> Value.TokenName -> TypedPolicy ()
authPolicy nft authTokenName =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledAuthTokenPolicy))
        [V2.toData nft, V2.toData authTokenName]

compiledAuthTokenPolicy :: PlutusTx.CompiledCode UntypedMintingPolicy
compiledAuthTokenPolicy =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-common/data/auth_token_policy.named-debruijn"))

authPolicySymbol :: Value.AssetClass -> Value.TokenName -> Ledger.CurrencySymbol
authPolicySymbol = (scriptCurrencySymbol .) . authPolicy
