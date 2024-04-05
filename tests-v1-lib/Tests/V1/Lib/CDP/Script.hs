{-# LANGUAGE TemplateHaskell #-}

module Tests.V1.Lib.CDP.Script
  ( CDPScript,
    CDPCreatorScript,
    cdpScript,
    cdpCreatorScript,
  )
where

import Indigo.V1.Contracts.CDP.Common
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
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

type CDPScript = TypedValidator CDPDatum CDPRedeemer

cdpScript :: CDPScriptParams -> CDPScript
cdpScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateCDPScript))
        [V2.toData params]

compiledValidateCDPScript ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateCDPScript =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/cdp.named-debruijn"))

type CDPCreatorScript = TypedValidator CDPCreatorDatum CDPCreatorRedeemer

cdpCreatorScript :: CDPCreatorScriptParams -> CDPCreatorScript
cdpCreatorScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateCDPCreatorScript))
        [V2.toData params]

compiledValidateCDPCreatorScript ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateCDPCreatorScript =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/cdp_creator.named-debruijn"))
