{-# LANGUAGE TemplateHaskell #-}

module Tests.V1.Lib.Collector.Script (CollectorScript, collectorScript) where

import Indigo.V1.Contracts.Collector.Common
  ( CollectorRedeemer,
    CollectorScriptParams,
  )
import Ledger qualified
import Plutus.Model.V2 (TypedValidator, mkTypedValidator')
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

type CollectorScript = TypedValidator () CollectorRedeemer

collectorScript :: CollectorScriptParams -> CollectorScript
collectorScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateCollector))
        [V2.toData params]

compiledValidateCollector :: PlutusTx.CompiledCode UntypedValidator
compiledValidateCollector =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/collector.named-debruijn"))
