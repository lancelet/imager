module Imager.Image
  ( -- * Types
    Image(Image, sample)
  ) where

import           Imager.Pt                      ( Pt )

newtype Image a b = Image { sample :: Pt a -> b }
