{-# LANGUAGE TemplateHaskell #-}

module Tests.VX.Lib.Staking.Script
  ( StakingScript,
    stakingScript,
    stakingValidatorHash,
  )
where

import Indigo.VX.Contracts.Staking.Common
  ( StakingDatum,
    StakingParams,
    StakingRedeemer,
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

type StakingScript = TypedValidator StakingDatum StakingRedeemer

stakingValidatorHash :: StakingParams -> Ledger.ValidatorHash
stakingValidatorHash params = Scripts.validatorHash $ untypedStakingScript params

untypedStakingScript :: StakingParams -> V2.Validator
untypedStakingScript params =
  V2.Validator $
    Ledger.applyArguments
      (Ledger.getValidator (V2.mkValidatorScript compiledValidateStaking))
      [V2.toData params]

stakingScript :: StakingParams -> StakingScript
stakingScript = mkTypedValidator' . untypedStakingScript

compiledValidateStaking ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateStaking =
  $$(loadValidatorWithFileWatching (projectDirectory <> "/tests-vX-lib/data/staking.named-debruijn"))
