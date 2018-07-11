-- | Surgery for generic data types:
-- remove and insert constructors and fields.

module Generic.Data.Surgery
  ( -- * Getting into the operating room
    OR

  , toOR
  , toData
  , fromData
  , fromOR

  , OROf

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

    -- ** Conversions

  , ToORRep
  , ToOR
  , FromORRep
  , FromOR

    -- ** Surgeries

  , RmvCField
  , InsCField
  , RmvRField
  , InsRField
  , RmvConstr
  , InsConstr
  ) where

import Generic.Data.Internal.Surgery
