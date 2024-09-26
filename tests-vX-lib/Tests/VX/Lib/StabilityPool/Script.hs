{-# LANGUAGE TemplateHaskell #-}

module Tests.VX.Lib.StabilityPool.Script
  ( StabilityPoolScript,
    stabilityPoolScript,
    stabilityPoolValidatorHash,
  )
where

import Indigo.VX.Contracts.StabilityPool.Common
  ( StabilityDatum,
    StabilityPoolParams,
    StabilityPoolRedeemer,
  )
import Ledger qualified
import Plutus.Model.V2 (TypedValidator, mkTypedValidator')
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
import Tests.Common.THUtils (projectDirectory)

type StabilityPoolScript = TypedValidator StabilityDatum StabilityPoolRedeemer

stabilityPoolValidatorHash :: StabilityPoolParams -> Ledger.ValidatorHash
stabilityPoolValidatorHash params = Scripts.validatorHash $ untypedStabilityPoolScript params

untypedStabilityPoolScript :: StabilityPoolParams -> V2.Validator
untypedStabilityPoolScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateStabilityPool))
      [V2.toData params]

stabilityPoolScript :: StabilityPoolParams -> StabilityPoolScript
stabilityPoolScript = mkTypedValidator' . untypedStabilityPoolScript

compiledValidateStabilityPool :: PlutusTx.CompiledCode UntypedValidator
compiledValidateStabilityPool =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/stability_pool.named-debruijn"))
