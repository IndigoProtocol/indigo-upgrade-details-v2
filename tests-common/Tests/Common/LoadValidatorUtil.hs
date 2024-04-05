module Tests.Common.LoadValidatorUtil (loadValidatorUsingEnvVar, loadPolicyUsingEnvVar) where

import Control.Monad (Monad ((>>)))
import Language.Haskell.TH.Syntax.Compat (SpliceQ)
import Ledger.Typed.Scripts (UntypedMintingPolicy, UntypedValidator)
import PlutusTx qualified
import Tests.Common.THUtils (doesFileExist', projectDirectory, watchFileChanges)
import Tests.Common.ValidatorsPath (validatorsPathFromEnv)
import Prelude (Semigroup ((<>)), String)

-- | Load validator from file and watch it for changes to recompile when a change happens.
-- The file should be in NamedDebruijn format.
-- In case such file unavailable, load an always fail validator.
--
-- NOTICE:  The validators directory loaded from the env variable can't be watched for changes.
--          In the case of the env variable change a manual recompilation is necessary.
loadValidatorUsingEnvVar :: String -> SpliceQ (PlutusTx.CompiledCode UntypedValidator)
loadValidatorUsingEnvVar fileName = loadValidator' fileName (projectDirectory  <> "/indigo-v2/scripts/" <> fileName)

-- | Load validator from file and watch it for changes to recompile when a change happens.
-- The file should be in NamedDebruijn format.
-- In case such file unavailable, load an always fail policy.
loadPolicyUsingEnvVar :: String -> SpliceQ (PlutusTx.CompiledCode UntypedMintingPolicy)
loadPolicyUsingEnvVar fileName = loadValidator' fileName (projectDirectory <> "/indigo-v2/scripts/" <> fileName)

loadValidator' :: String -> String -> SpliceQ (PlutusTx.CompiledCode a)
loadValidator' fileName fallbackValidatorPath =
  let pathUsingEnv = validatorsPathFromEnv <> fileName
   in do
        exists <- doesFileExist' pathUsingEnv
        let path = if exists then pathUsingEnv else fallbackValidatorPath
        watchFileChanges path >> PlutusTx.loadFromFile path
