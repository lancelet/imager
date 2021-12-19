{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Imager.Rect
Description : Rectangle

An endpoint-exclusive rectangle.
-}
module Imager.Rect
  ( -- * Types
    Rect
    -- * Functions
  , rect
  , minX
  , maxX
  , minY
  , maxY
  , contains
  ) where

import           Imager.Pt                      ( Pt )
import qualified Imager.Pt                     as Pt

import           Data.List                      ( intercalate )
import           Data.Strict.Maybe              ( Maybe(Just, Nothing)
                                                , catMaybes
                                                )
import           Prelude                 hiding ( Maybe(..) )


-- | An endpoint-exclusive rectangle.
--
-- A @Rect@ segments a 2D plane into an included region and an excluded region.
-- The included region of the rect is defined by all points @(x, y)@, such that:
--
-- @
-- ('minX' <= x < 'maxX') && ('minY' <= y < 'maxY')
-- @
--
-- The rectangle does not include points along its @maxX@ or @maxY@ boundary.
-- This is so that the width and height of integer rectangles is correct, and so
-- that integer rectangles scale properly.
data Rect a = Rect
  { minX :: !a  -- ^ Minimum x value.
  , maxX :: !a  -- ^ Maximum x value.
  , minY :: !a  -- ^ Minimum y value.
  , maxY :: !a  -- ^ Maximum y value.
  }
  deriving stock (Eq, Show)


-- | Create a 'Rect'.
--
-- The minimum and maximum values of the @Rect@ must be sorted correctly. A
-- @Rect@ can be created like this:
--
-- >>> rect 10 20 100 200 :: Rect Int
-- Rect {minX = 10, maxX = 20, minY = 100, maxY = 200}
--
-- But if either of the bounds are not ordered correctly, an exception will be
-- thrown:
--
-- >>> rect 20 10 200 100 :: Rect Int
-- ... Invalid Rect: maxX (10) < minX (20) and maxY (100) < minY (200)
-- ...
rect
  :: forall a
   . (Show a, Ord a)
  => a       -- ^ minimum x value
  -> a       -- ^ maximum x value
  -> a       -- ^ minimum y value
  -> a       -- ^ maximum y value
  -> Rect a  -- ^ new rectangle
rect minX maxX minY maxY = if (maxX < minX) || (maxY < minY)
  then
    let
      xErr, yErr :: Maybe String
      xErr = if maxX < minX
        then Just $ "maxX (" ++ show maxX ++ ") < minX (" ++ show minX ++ ")"
        else Nothing
      yErr = if maxY < minY
        then Just $ "maxY (" ++ show maxY ++ ") < minY (" ++ show minY ++ ")"
        else Nothing

      details :: String
      details = intercalate " and " . catMaybes $ [xErr, yErr]
    in
      error $ "Invalid Rect: " ++ details
  else Rect { .. }


-- | Check if a 'Rect' contains a 'Pt'.
--
-- >>> import Imager.Pt (pt)
-- >>> r = Rect 10 20 100 200
-- >>> contains r (pt 15 150)
-- True
-- >>> contains r (pt 20 150)
-- False
contains
  :: forall a
   . (Ord a)
  => Rect a  -- ^ rectangle to test for point containment
  -> Pt a    -- ^ point to test
  -> Bool    -- ^ true if the rectangle contains the point; false otherwise
contains Rect {..} p =
  let x, y :: a
      x = Pt.x p
      y = Pt.y p
  in  (minX <= x) && (x < maxX) && (minY <= y) && (y < maxY)
