{-# LANGUAGE TemplateHaskell #-}

module Tests.VX.Lib.Governance.Script
  ( GovScript,
    govScript,
    govValidatorHash,
    PollShardScript,
    PollManagerScript,
    pollManagerAddress,
    pollShardScript,
    pollShardValidatorHash,
    pollManagerScript,
    pollManagerValidatorHash,
    ExecuteScript,
    executeScript,
    executeValidatorHash,
    VersionRegistryScript,
    versionRegistryScript,
    versionRegistryValidatorHash,
    versionRecordPolicy,
    versionRecordSymbol,
  )
where

import Indigo.VX.Contracts.Governance.Execute.Common
  ( ExecuteParams,
    ExecuteRedeemer,
    Upgrade,
  )
import Indigo.VX.Contracts.Governance.Gov.Common (GovDatum, GovParams, GovRedeemer)
import Indigo.VX.Contracts.Governance.Poll.Common
  ( PollManager,
    PollManagerParams,
    PollManagerRedeemer,
    PollParams,
    PollRedeemer,
    PollShard,
  )
import Indigo.VX.Contracts.Governance.VersionRegistry.Common
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
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Tests.Common.LoadValidatorUtil (loadValidatorWithFileWatching)
import Tests.Common.THUtils (projectDirectory)

type GovScript = TypedValidator GovDatum GovRedeemer

govValidatorHash :: GovParams -> Ledger.ValidatorHash
govValidatorHash params = Scripts.validatorHash $ untypedGovScript params

untypedGovScript :: GovParams -> V2.Validator
untypedGovScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateGov))
      [V2.toData params]

govScript :: GovParams -> GovScript
govScript = mkTypedValidator' . untypedGovScript

compiledValidateGov ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateGov =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/gov.named-debruijn"))

type PollShardScript = TypedValidator PollShard PollRedeemer

type PollManagerScript = TypedValidator PollManager PollManagerRedeemer

pollShardValidatorHash :: PollParams -> Ledger.ValidatorHash
pollShardValidatorHash params = Scripts.validatorHash $ untypedPollShardScript params

untypedPollShardScript :: PollParams -> V2.Validator
untypedPollShardScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidatePollShard))
      [V2.toData params]

pollShardScript :: PollParams -> PollShardScript
pollShardScript = mkTypedValidator' . untypedPollShardScript

compiledValidatePollShard :: PlutusTx.CompiledCode UntypedValidator
compiledValidatePollShard =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/poll_shard.named-debruijn"))

pollManagerValidatorHash :: PollManagerParams -> Ledger.ValidatorHash
pollManagerValidatorHash params = Scripts.validatorHash $ untypedPollManagerScript params

untypedPollManagerScript :: PollManagerParams -> V2.Validator
untypedPollManagerScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidatePollManager))
      [V2.toData params]

pollManagerScript :: PollManagerParams -> PollManagerScript
pollManagerScript = mkTypedValidator' . untypedPollManagerScript

compiledValidatePollManager ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidatePollManager =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/poll_manager.named-debruijn"))

pollManagerAddress :: PollManagerParams -> Ledger.Address
pollManagerAddress =
  Ledger.scriptHashAddress . validatorHash . pollManagerScript

type ExecuteScript = TypedValidator Upgrade ExecuteRedeemer

executeValidatorHash :: ExecuteParams -> Ledger.ValidatorHash
executeValidatorHash params = Scripts.validatorHash $ untypedExecuteScript params

untypedExecuteScript :: ExecuteParams -> V2.Validator
untypedExecuteScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateExecute))
      [V2.toData params]

executeScript :: ExecuteParams -> ExecuteScript
executeScript = mkTypedValidator' . untypedExecuteScript

compiledValidateExecute ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateExecute =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/execute.named-debruijn"))

type VersionRegistryScript = TypedValidator VersionRecord VersionRecordRedeemer

versionRegistryValidatorHash :: Ledger.ValidatorHash
versionRegistryValidatorHash = Scripts.validatorHash untypedVersionRegistryScript

untypedVersionRegistryScript :: V2.Validator
untypedVersionRegistryScript =
  V2.Validator $ Ledger.getValidator (V2.mkValidatorScript compiledVersionRegistryScript)

versionRegistryScript :: VersionRegistryScript
versionRegistryScript = mkTypedValidator' untypedVersionRegistryScript

compiledVersionRegistryScript ::
  PlutusTx.CompiledCode UntypedValidator
compiledVersionRegistryScript =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/version_registry.named-debruijn"))

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
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/version_registry_policy.named-debruijn"))

versionRecordSymbol :: VersionRecordParams -> V2.CurrencySymbol
versionRecordSymbol = scriptCurrencySymbol . versionRecordPolicy
