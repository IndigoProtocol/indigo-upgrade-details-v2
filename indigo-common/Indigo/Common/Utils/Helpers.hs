module Indigo.Common.Utils.Helpers
  ( hasUnitValue,
    unitValue,
    oneSecond,
    oneYear,
  )
where

import Ledger qualified
import Ledger.Value qualified as Value
import PlutusTx.Prelude

{-# INLINEABLE oneSecond #-}
oneSecond :: Ledger.POSIXTime
oneSecond = Ledger.POSIXTime 1_000

{-# INLINEABLE oneYear #-}
oneYear :: Ledger.POSIXTime
oneYear = Ledger.POSIXTime 31_536_000_000

{-# INLINEABLE unitValue #-}
unitValue :: Value.AssetClass -> Ledger.Value
unitValue ac = Value.assetClassValue ac 1

{-# INLINEABLE hasUnitValue #-}
hasUnitValue :: Ledger.Value -> Value.AssetClass -> Bool
hasUnitValue v ac = Value.assetClassValueOf v ac == 1
