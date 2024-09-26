{-# LANGUAGE TemplateHaskell #-}

module Tests.V2.Lib.Staking.Script
  ( StakingScript,
    stakingScript,
    stakingValidatorHash,
  )
where

import Indigo.V2.Contracts.Staking.Common
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
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v2-lib/data/staking.named-debruijn"))
