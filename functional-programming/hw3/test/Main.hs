module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import ConcurrentHashTableSpec (hashTableTests)
import ScriptSpec (scriptTests)
import ScriptPrinterSpec (printTests)


main :: IO ()
main = defaultMain $
  testGroup "hw3 tests" [scriptTests, printTests, hashTableTests]
