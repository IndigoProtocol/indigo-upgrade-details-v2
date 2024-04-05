{-# LANGUAGE TemplateHaskell #-}

module Tests.V1.Lib.StabilityPool.Script
  ( StabilityPoolScript,
    stabilityPoolScript,
  )
where

import Indigo.V1.Contracts.StabilityPool.Common
  ( StabilityDatum,
    StabilityPoolParams,
    StabilityPoolRedeemer,
  )
import Ledger qualified
import Plutus.Model.V2 (TypedValidator, mkTypedValidator')
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

type StabilityPoolScript = TypedValidator StabilityDatum StabilityPoolRedeemer

stabilityPoolScript :: StabilityPoolParams -> StabilityPoolScript
stabilityPoolScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateStabilityPool))
        [V2.toData params]

compiledValidateStabilityPool :: PlutusTx.CompiledCode UntypedValidator
compiledValidateStabilityPool =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/stability_pool.named-debruijn"))
