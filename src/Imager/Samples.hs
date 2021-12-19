module Imager.Samples
  ( -- * Types
    Sample(Sample, pt, value)
  , Samples(Samples, samples)
  ) where

import           Imager.Pt                      ( Pt )
import           Imager.Rect                    ( Rect )

newtype Samples a b = Samples { samples :: Rect a -> [Sample a b] }

data Sample a b = Sample
  { pt    :: !(Pt a)
  , value :: !a
  }
