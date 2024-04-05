{-# LANGUAGE TemplateHaskell #-}

module Tests.V1.Lib.Governance.Script
  ( GovScript,
    govScript,
    PollScript,
    PollManagerScript,
    pollManagerAddress,
    pollScript,
    pollManagerScript,
    ExecuteScript,
    executeScript,
    VersionRegistryScript,
    versionRegistryScript,
    versionRecordPolicy,
    versionRecordSymbol,
  )
where

import Indigo.V1.Contracts.Governance.Execute.Common
  ( ExecuteParams,
    ExecuteRedeemer,
    Upgrade,
  )
import Indigo.V1.Contracts.Governance.Gov.Common (GovDatum, GovParams, GovRedeemer)
import Indigo.V1.Contracts.Governance.Poll.Common
  ( PollManager,
    PollManagerParams,
    PollManagerRedeemer,
    PollParams,
    PollRedeemer,
    PollShard,
  )
import Indigo.V1.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord,
    VersionRecordMintingPolicyRedeemer,
    VersionRecordParams,
    VersionRecordRedeemer,
  )
import Ledger qualified
import Ledger.Typed.Scripts (UntypedMintingPolicy)
import Plutus.Model.V2
  ( TypedPolicy,
    TypedValidator,
    mkTypedPolicy',
    mkTypedValidator',
    scriptCurrencySymbol,
    validatorHash,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.THUtils (projectDirectory)

type GovScript = TypedValidator GovDatum GovRedeemer

govScript :: GovParams -> GovScript
govScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateGov))
        [V2.toData params]

compiledValidateGov ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateGov =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/gov.named-debruijn"))

type PollScript = TypedValidator PollShard PollRedeemer

type PollManagerScript = TypedValidator PollManager PollManagerRedeemer

pollScript :: PollParams -> PollScript
pollScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidatePoll))
        [V2.toData params]

compiledValidatePoll :: PlutusTx.CompiledCode UntypedValidator
compiledValidatePoll =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/poll.named-debruijn"))

pollManagerScript :: PollManagerParams -> PollManagerScript
pollManagerScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidatePollManager))
        [V2.toData params]

compiledValidatePollManager ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidatePollManager =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/poll_manager.named-debruijn"))

pollManagerAddress :: PollManagerParams -> Ledger.Address
pollManagerAddress =
  Ledger.scriptHashAddress . validatorHash . pollManagerScript

type ExecuteScript = TypedValidator Upgrade ExecuteRedeemer

executeScript :: ExecuteParams -> ExecuteScript
executeScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateExecute))
        [V2.toData params]

compiledValidateExecute ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateExecute =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/execute.named-debruijn"))

type VersionRegistryScript = TypedValidator VersionRecord VersionRecordRedeemer

versionRegistryScript :: VersionRegistryScript
versionRegistryScript =
  mkTypedValidator' $
    V2.Validator (Ledger.getValidator (V2.mkValidatorScript compiledVersionRegistryScript))

compiledVersionRegistryScript ::
  PlutusTx.CompiledCode UntypedValidator
compiledVersionRegistryScript =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/version_registry.named-debruijn"))

versionRecordPolicy ::
  VersionRecordParams -> TypedPolicy VersionRecordMintingPolicyRedeemer
versionRecordPolicy params =
  mkTypedPolicy' $
    V2.MintingPolicy $
      Ledger.applyArguments
        (Ledger.getMintingPolicy (V2.mkMintingPolicyScript compiledVersionRecordPolicy))
        [V2.toData params]

compiledVersionRecordPolicy :: PlutusTx.CompiledCode UntypedMintingPolicy
compiledVersionRecordPolicy =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/version_registry_policy.named-debruijn"))

versionRecordSymbol :: VersionRecordParams -> V2.CurrencySymbol
versionRecordSymbol = scriptCurrencySymbol . versionRecordPolicy
