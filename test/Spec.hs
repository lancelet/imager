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
runDocTests = DocTest.doctest ["-isrc", "src/Imager/Rect.hs"]
