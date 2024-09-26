{-# LANGUAGE TemplateHaskell #-}

module Tests.VX.Lib.CDP.Script
  ( CDPScript,
    CDPCreatorScript,
    cdpScript,
    cdpValidatorHash,
    cdpCreatorScript,
    cdpCreatorValidatorHash,
  )
where

import Indigo.VX.Contracts.CDP.Common
  ( CDPCreatorDatum,
    CDPCreatorRedeemer,
    CDPCreatorScriptParams,
    CDPDatum,
    CDPRedeemer,
    CDPScriptParams,
  )
import Ledger qualified
import Plutus.Model.V2
  ( TypedValidator,
    mkTypedValidator',
  )
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
import Tests.Common.THUtils (projectDirectory)

type CDPScript = TypedValidator CDPDatum CDPRedeemer

cdpValidatorHash :: CDPScriptParams -> Ledger.ValidatorHash
cdpValidatorHash params = Scripts.validatorHash $ untypedCDPScript params

untypedCDPScript :: CDPScriptParams -> V2.Validator
untypedCDPScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateCDPScript))
      [V2.toData params]

cdpScript :: CDPScriptParams -> CDPScript
cdpScript = mkTypedValidator' . untypedCDPScript

compiledValidateCDPScript ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateCDPScript =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/cdp.named-debruijn"))

type CDPCreatorScript = TypedValidator CDPCreatorDatum CDPCreatorRedeemer

cdpCreatorValidatorHash :: CDPCreatorScriptParams -> Ledger.ValidatorHash
cdpCreatorValidatorHash params = Scripts.validatorHash $ untypedCDPCreatorScript params

untypedCDPCreatorScript :: CDPCreatorScriptParams -> V2.Validator
untypedCDPCreatorScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateCDPCreatorScript))
      [V2.toData params]

cdpCreatorScript :: CDPCreatorScriptParams -> CDPCreatorScript
cdpCreatorScript = mkTypedValidator' . untypedCDPCreatorScript

compiledValidateCDPCreatorScript ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateCDPCreatorScript =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/cdp_creator.named-debruijn"))
