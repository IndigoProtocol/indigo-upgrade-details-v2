{-# LANGUAGE TemplateHaskell #-}

module Tests.V1.Lib.Treasury.Script (TreasuryScript, treasuryScript, treasuryScriptAddress) where

import Indigo.V1.Contracts.Treasury.Common (TreasuryRedeemer, TreasuryScriptParams)
import Ledger qualified
import Plutus.Model.V2
  ( TypedValidator,
    mkTypedValidator',
    validatorHash,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

type TreasuryScript = TypedValidator () TreasuryRedeemer

treasuryScript :: TreasuryScriptParams -> TreasuryScript
treasuryScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateTreasuryScript))
        [V2.toData params]

compiledValidateTreasuryScript ::
  PlutusTx.CompiledCode Validators.UntypedValidator
compiledValidateTreasuryScript =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/treasury.named-debruijn"))

treasuryScriptAddress :: TreasuryScriptParams -> V2.Address
treasuryScriptAddress =
  Ledger.scriptHashAddress . validatorHash . treasuryScript
