{-# LANGUAGE TemplateHaskell #-}

module Tests.V1.Lib.Staking.Script
  ( StakingScript,
    stakingScript,
  )
where

import Indigo.V1.Contracts.Staking.Common
  ( StakingDatum,
    StakingParams,
    StakingRedeemer,
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

type StakingScript = TypedValidator StakingDatum StakingRedeemer

stakingScript :: StakingParams -> StakingScript
stakingScript params =
  mkTypedValidator' $
    V2.Validator $
      Ledger.applyArguments
        (Ledger.getValidator (V2.mkValidatorScript compiledValidateStaking))
        [V2.toData params]

compiledValidateStaking ::
  PlutusTx.CompiledCode UntypedValidator
compiledValidateStaking =
  $$(PlutusTx.loadFromFile (projectDirectory <> "/tests-v1-lib/data/staking.named-debruijn"))
