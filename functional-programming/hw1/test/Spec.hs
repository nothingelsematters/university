{-# LANGUAGE OverloadedStrings #-}

module Spec
  ( main
  ) where

import SpecBlock1 (firstBlockTest)
import SpecBlock2 (secondBlockTest)
import SpecBlock3 (thirdBlockTest)
import SpecBlock4 (fourthBlockTest)
import SpecBlock5 (fifthBlockTest)
import SpecBlock6 (sixthBlockTest)

import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = defaultMain . testGroup "First home work tests" $
  [ firstBlockTest
  , secondBlockTest
  , thirdBlockTest
  , fourthBlockTest
  , fifthBlockTest
  , sixthBlockTest
  ]
