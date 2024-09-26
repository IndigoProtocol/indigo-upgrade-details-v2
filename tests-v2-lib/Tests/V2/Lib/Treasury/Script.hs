{-# LANGUAGE TemplateHaskell #-}

module Tests.V2.Lib.Treasury.Script
  ( TreasuryScript,
    treasuryScript,
    treasuryScriptAddress,
    treasuryValidatorHash,
  )
where

import Indigo.V2.Contracts.Treasury.Common (TreasuryRedeemer, TreasuryScriptParams)
import Ledger qualified
import Plutus.Model.V2
  ( TypedValidator,
    mkTypedValidator',
    validatorHash,
  )
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

type TreasuryScript = TypedValidator () TreasuryRedeemer

treasuryValidatorHash :: TreasuryScriptParams -> Ledger.ValidatorHash
treasuryValidatorHash params = Scripts.validatorHash $ untypedTreasuryScript params

untypedTreasuryScript :: TreasuryScriptParams -> V2.Validator
untypedTreasuryScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateTreasuryScript))
      [V2.toData params]

treasuryScript :: TreasuryScriptParams -> TreasuryScript
treasuryScript = mkTypedValidator' . untypedTreasuryScript

compiledValidateTreasuryScript ::
  PlutusTx.CompiledCode Validators.UntypedValidator
compiledValidateTreasuryScript =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v2-lib/data/treasury.named-debruijn"))

treasuryScriptAddress :: TreasuryScriptParams -> V2.Address
treasuryScriptAddress =
  Ledger.scriptHashAddress . validatorHash . treasuryScript
