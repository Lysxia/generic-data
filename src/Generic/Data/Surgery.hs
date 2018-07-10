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
