{-# LANGUAGE TemplateHaskell #-}

module Tests.Common.ValidatorsPath (validatorsPathFromEnv) where

import Tests.Common.THUtils (envQ')
import Prelude qualified as P

validatorsPathFromEnv :: P.String
validatorsPathFromEnv = $$(envQ' "VALIDATORS_PATH")
