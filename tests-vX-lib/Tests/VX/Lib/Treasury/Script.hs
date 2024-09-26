{-# LANGUAGE TemplateHaskell #-}

module Tests.VX.Lib.Treasury.Script
  ( TreasuryScript,
    treasuryScript,
    treasuryScriptAddress,
    treasuryValidatorHash,
  )
where

import Indigo.VX.Contracts.Treasury.Common (TreasuryRedeemer, TreasuryScriptParams, treasuryUtxosStakeCredential)
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
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
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
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/treasury.named-debruijn"))

treasuryScriptAddress :: TreasuryScriptParams -> V2.Address
treasuryScriptAddress param =
  let vh = validatorHash (treasuryScript param)
      sc = treasuryUtxosStakeCredential param
  in Ledger.Address (V2.ScriptCredential vh) sc
