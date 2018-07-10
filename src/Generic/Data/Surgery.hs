module Generic.Data.Surgery
  ( -- * Operation room
    LoL
  , toLoL
  , toData
  , fromData
  , fromLoL

  , LoLOf

    -- * Surgeries
  , removeCField
  , insertCField
  , removeRField
  , insertRField
  , removeConstr
  , insertConstr

    -- * Constraint synonyms

    -- | Hiding implementation details from the signatures above.
    -- Useful to compose surgeries in a reusable way.

  , ToLoLRep
  , ToLoL
  , FromLoLRep
  , FromLoL

  , RmvCField
  , InsCField
  , RmvRField
  , InsRField
  , RmvConstr
  , InsConstr
  ) where

import Generic.Data.Internal.Surgery
