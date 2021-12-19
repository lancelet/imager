module Main (main) where

import Imager (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
