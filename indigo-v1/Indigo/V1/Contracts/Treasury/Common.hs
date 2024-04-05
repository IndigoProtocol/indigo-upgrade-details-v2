{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.V1.Contracts.Treasury.Common
  ( TreasuryScriptParams (MkTreasuryScriptParams, versionRecordToken),
    TreasuryScript,
    TreasuryRedeemer (UpgradeVersion),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Orphans ()
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import Prelude qualified as P

data TreasuryScriptParams = MkTreasuryScriptParams
  { versionRecordToken :: Value.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''TreasuryScriptParams
PlutusTx.makeIsDataIndexed ''TreasuryScriptParams [('MkTreasuryScriptParams, 0)]

data TreasuryRedeemer = UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''TreasuryRedeemer
PlutusTx.makeIsDataIndexed ''TreasuryRedeemer [('UpgradeVersion, 0)]

data TreasuryScript

instance TScripts.ValidatorTypes TreasuryScript where
  type DatumType TreasuryScript = ()
  type RedeemerType TreasuryScript = TreasuryRedeemer
