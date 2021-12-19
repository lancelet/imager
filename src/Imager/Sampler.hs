{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Imager.Sampler where

import           Imager.Image                   ( Image )
import           Imager.Pt                      ( Pt )
import           Imager.Rect                    ( Rect )
import qualified Imager.Rect                   as Rect
import           Imager.Samples                 ( Sample )
import qualified Imager.Util                   as Util

import           Data.Vector.Generic            ( Vector )
import           Data.Word                      ( Word16
                                                , Word32
                                                )


data PixelSamples = PixelSamples
  { nSamplesX :: !Word16
  , nSamplesY :: !Word16
  }
  deriving stock (Eq, Show)

data Jitter a = Jitter
  { jitterX :: !a
  , jitterY :: !a
  }
  deriving stock (Eq, Show)

mkJitter :: forall a . (Ord a, Fractional a) => a -> a -> Jitter a
mkJitter x y =
  Jitter { jitterX = Util.clamp 0.0 1.0 x, jitterY = Util.clamp 0.0 1.0 y }

data UniformSamples v a b = UniformSamples
  { usRect    :: !(Rect Word16)
  , usPxSamp  :: !PixelSamples
  , usSamples :: !(v (Sample a b))
  }

sampleUniform
  :: forall v a b
   . (Vector v (Sample a b))
  => PixelSamples               -- ^ number of samples in x and y
  -> Jitter a                   -- ^ jitter amount in x and y
  -> Rect Word16                -- ^ region of the image to sample (pixel rect)
  -> Image a b                  -- ^ the image to sample
  -> UniformSamples v a b  -- ^ samples from the image
sampleUniform PixelSamples {..} Jitter {..} region image =
  let rgnw, rgnh :: Word16
      rgnw = Rect.width region
      rgnh = Rect.height region

      arrLen :: Word32
      arrLen =
        fromIntegral rgnw
          * fromIntegral rgnh
          * fromIntegral nSamplesX
          * fromIntegral nSamplesY
  in  undefined
