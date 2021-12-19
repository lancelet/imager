module Main
  ( main
  ) where

import           Imager                         ( projectName )


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
