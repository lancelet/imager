{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Imager.Util
  ( -- * Functions
    clamp
  ) where

import           Prelude                        ( (<)
                                                , (>)
                                                , Ord
                                                , otherwise
                                                )


-- | Clamp a value to an inclusive range.
--
-- >>> clamp 5 10 6
-- 6
-- >>> clamp 5 10 4
-- 5
-- >>> clamp 5 10 50
-- 10
clamp
  :: forall a
   . Ord a
  => a  -- ^ minimum allowed value
  -> a  -- ^ maximum allowed value
  -> a  -- ^ value to clamp
  -> a  -- ^ clamped value
clamp min max x | x < min   = min
                | x > max   = max
                | otherwise = x
{-# INLINABLE clamp #-}
