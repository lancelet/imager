{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Imager.Pt
Description : Point

A point in 2D space
-}
module Imager.Pt
  ( -- * Types
    Pt
    -- * Functions
  , pt
  , x
  , y
  ) where

-- | A point in 2D space.
data Pt a = Pt
  { x :: !a  -- ^ x coordinate value.
  , y :: !a  -- ^ y coordinate value.
  }
  deriving stock (Eq, Show)

-- | Create a point.
--
-- >>> pt 42 43
-- Pt {x = 42, y = 43}
pt :: forall a. a -> a -> Pt a
pt x y = Pt {..}
