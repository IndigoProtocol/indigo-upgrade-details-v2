{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.VX.Contracts.Treasury.Common
  ( TreasuryScriptParams
      ( MkTreasuryScriptParams,
        upgradeToken,
        versionRecordToken,
        treasuryUtxosStakeCredential
      ),
    TreasuryScript,
    TreasuryRedeemer
      ( Withdraw,
        PrepareWithdrawal,
        Split,
        Merge,
        CollectAda,
        UpgradeVersion
      ),
  )
where

import GHC.Generics (Generic)
import Ledger.Orphans ()
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as P

data TreasuryScriptParams = MkTreasuryScriptParams
  { -- | Token authenticating passed proposal.
    upgradeToken :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass,
    treasuryUtxosStakeCredential :: Maybe V2.StakingCredential
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''TreasuryScriptParams
PlutusTx.makeIsDataIndexed ''TreasuryScriptParams [('MkTreasuryScriptParams, 0)]

data TreasuryRedeemer
  = Withdraw
  | PrepareWithdrawal
  | Split
  | Merge
  | CollectAda
  | UpgradeVersion
  deriving stock (P.Show, Generic)

PlutusTx.makeLift ''TreasuryRedeemer
PlutusTx.makeIsDataIndexed
  ''TreasuryRedeemer
  [ ('Withdraw, 0),
    ('PrepareWithdrawal, 1),
    ('Split, 2),
    ('Merge, 3),
    ('CollectAda, 4),
    ('UpgradeVersion, 5)
  ]

data TreasuryScript

instance TScripts.ValidatorTypes TreasuryScript where
  type DatumType TreasuryScript = ()
  type RedeemerType TreasuryScript = TreasuryRedeemer
