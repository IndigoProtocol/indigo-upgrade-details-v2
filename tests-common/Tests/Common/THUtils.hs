{-# LANGUAGE TemplateHaskell #-}

module Tests.Common.THUtils
  ( envQ',
    watchFileChanges,
    doesFileExist',
    projectDirectory,
  )
where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.String (IsString (fromString))
import Language.Haskell.TH (Q, litE, runIO, stringL)
import Language.Haskell.TH.Syntax (Lift (liftTyped), addDependentFile)
import Language.Haskell.TH.Syntax.Compat (IsCode (fromCode, toCode), SpliceQ, liftSplice)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import Prelude (Bool, Maybe (Just, Nothing), Monad ((>>=)), String, ($), (.))

-- | Watch file for content changes.
-- When the file contents change, recompile this splice.
watchFileChanges :: String -> SpliceQ ()
watchFileChanges filePath =
  liftSplice $
    addDependentFile filePath >>= liftTyped

doesFileExist' :: String -> Q Bool
doesFileExist' filePath =
  liftIO (doesFileExist filePath)

-- | Copied from package https://hackage.haskell.org/package/th-env
envQ' ::
  IsString a =>
  -- | Environment variable name.
  String ->
  SpliceQ a
envQ' name =
  liftSplice $
    runIO (lookupEnv name) >>= \case
      Just v -> fromCode $ toCode [||fromString v||]
      Nothing -> fromCode $ toCode [||""||]

-- fail $ "Environment variable " ++ name ++ " is not set"

projectDirectory :: String
projectDirectory = $(runIO getCurrentDirectory >>= litE . stringL)
