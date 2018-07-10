module Generic.Data.Surgery
  ( -- * Operation room
    LoL
  , toLoL
  , toData
  , fromData
  , fromLoL

    -- * Surgeries
  , removeCField
  , insertCField
  , removeRField
  , insertRField
  , removeConstr
  , insertConstr
  ) where

import Generic.Data.Internal.Surgery
