{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common data structures for CDP Upgrade Script.
-}

module Tests.Upgrades.VX.Lib.CDP.Types (CDPUpgradeRedeemer (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import PlutusTx qualified
import Prelude qualified as P

data CDPUpgradeRedeemer
  = UpgradeCDP
  | UpgradeIAsset
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CDPUpgradeRedeemer
PlutusTx.makeIsDataIndexed
  ''CDPUpgradeRedeemer
  [('UpgradeCDP, 0), ('UpgradeIAsset, 1)]
