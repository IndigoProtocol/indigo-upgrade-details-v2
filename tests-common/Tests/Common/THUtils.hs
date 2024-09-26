{-# LANGUAGE TemplateHaskell #-}

module Tests.Common.THUtils
  ( watchFileChanges,
    projectDirectory,
  )
where

import Language.Haskell.TH (litE, runIO, stringL)
import Language.Haskell.TH.Syntax (Lift (liftTyped), addDependentFile)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, liftSplice)
import System.Directory (getCurrentDirectory)
import Prelude (Monad ((>>=)), String, ($), (.))

-- | Watch file for content changes.
-- When the file contents change, recompile this splice.
watchFileChanges :: String -> SpliceQ ()
watchFileChanges filePath =
  liftSplice $
    addDependentFile filePath >>= liftTyped

projectDirectory :: String
projectDirectory = $(runIO getCurrentDirectory >>= litE . stringL)
