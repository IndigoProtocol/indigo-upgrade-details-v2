{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common functions, data structures for CDP Script.
-}

module Indigo.V2.Contracts.Collector.Common
  ( CollectorScriptParams
      ( CollectorScriptParams,
        stakingManagerNFT,
        stakingToken,
        versionRecordToken
      ),
    CollectorRedeemer
      ( Collect,
        DistributeToStakers,
        UpgradeVersion
      ),
    CollectorScript,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Orphans ()
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import Prelude qualified as P

{-
Parameters of Collector Script.
-}
data CollectorScriptParams = CollectorScriptParams
  { stakingManagerNFT :: Value.AssetClass,
    stakingToken :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''CollectorScriptParams
PlutusTx.makeIsDataIndexed ''CollectorScriptParams [('CollectorScriptParams, 0)]

data CollectorRedeemer
  = Collect
  | DistributeToStakers
  | UpgradeVersion
  deriving stock (P.Eq, P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CollectorRedeemer
PlutusTx.makeIsDataIndexed
  ''CollectorRedeemer
  [('Collect, 0), ('DistributeToStakers, 1), ('UpgradeVersion, 2)]

data CollectorScript

instance TScripts.ValidatorTypes CollectorScript where
  type DatumType CollectorScript = ()
  type RedeemerType CollectorScript = CollectorRedeemer
