module Main
  ( main
  ) where

import           Test.DocTest                  as DocTest
                                                ( doctest )

main :: IO ()
main = announceRunDocTests

---------------------------------------------------------------------- DocTests

announceRunDocTests :: IO ()
announceRunDocTests = do
  putStrLn "\n"
  putStrLn "---- Running DocTests ----"
  runDocTests
  putStrLn "---- Finished DocTests ----"

runDocTests :: IO ()
runDocTests = DocTest.doctest
  [ "-isrc"
  , "src/Imager/Image.hs"
  , "src/Imager/Pt.hs"
  , "src/Imager/Rect.hs"
  , "src/Imager/Sampler.hs"
  , "src/Imager/Samples.hs"
  , "src/Imager/Util.hs"
  ]
