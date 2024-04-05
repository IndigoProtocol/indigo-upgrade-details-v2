{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.V2.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord (VersionRecord, versionId, versionPaths),
    VersionRecordParams (VersionRecordParams, upgradeToken),
    VersionRecordRedeemer,
    VersionRecordScript,
    VersionRecordMintingPolicyRedeemer (AddRecord),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude
import Prelude qualified as P

data VersionRecord = VersionRecord
  { versionId :: Integer,
    versionPaths :: Map ValidatorHash Value.CurrencySymbol
  }
  deriving (Generic, P.Show, P.Eq, ToJSON, FromJSON)

-- deriveEq ''VersionRecord
PlutusTx.makeLift ''VersionRecord
PlutusTx.makeIsDataIndexed ''VersionRecord [('VersionRecord, 0)]

-- TODO: use `deriveEq ''VersionRecord` of
-- https://github.com/Liqwid-Labs/plutus-extra
instance Eq VersionRecord where
  a == b =
    versionId a == versionId b
      && versionPaths a == versionPaths b

data VersionRecordParams = VersionRecordParams
  { upgradeToken :: Value.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''VersionRecordParams
PlutusTx.makeIsDataIndexed ''VersionRecordParams [('VersionRecordParams, 0)]

type VersionRecordRedeemer = Void

data VersionRecordMintingPolicyRedeemer = AddRecord
  deriving (Generic, P.Show, P.Eq, ToJSON, FromJSON)

PlutusTx.makeLift ''VersionRecordMintingPolicyRedeemer
PlutusTx.makeIsDataIndexed
  ''VersionRecordMintingPolicyRedeemer
  [('AddRecord, 0)]

data VersionRecordScript

instance TScripts.ValidatorTypes VersionRecordScript where
  type DatumType VersionRecordScript = VersionRecord
  type RedeemerType VersionRecordScript = VersionRecordRedeemer
