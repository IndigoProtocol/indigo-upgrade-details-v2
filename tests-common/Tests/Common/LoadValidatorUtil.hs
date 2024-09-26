module Tests.Common.LoadValidatorUtil
  ( loadValidatorWithFileWatching,
  )
where

import Control.Monad (Monad ((>>)))
import Language.Haskell.TH.Syntax.Compat (SpliceQ)
import PlutusTx qualified
import Tests.Common.THUtils (watchFileChanges)
import Prelude (String)

loadValidatorWithFileWatching :: String -> SpliceQ (PlutusTx.CompiledCode a)
loadValidatorWithFileWatching path = watchFileChanges path >> PlutusTx.loadFromFile path