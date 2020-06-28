{-# LANGUAGE BlockArguments #-}

module ScriptPrinterSpec
  ( printTests
  ) where


import Data.List (intercalate)
import Hedgehog (Property, diff, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Script (Script (..))
import ScriptPrinter (Printer, view)


testEquality :: Printer a -> [String] -> Property
testEquality script = property . diff (view script) (==) . intercalate "\n"

threeTypes :: Script a => a String
threeTypes =
  var (int 10) \a ->
  var (string "sample program with 3 types: 10 ") \result ->
  sIf (deref a @> raw 2)
    (result @= deref result @++ raw "is more than 2")
    (result @= deref result @++ raw "is less than 2 or equals 2")
    #
  deref result

threeTypesResult :: [String]
threeTypesResult =
 [ "var x0 = 10;"
 , "var x1 = \"sample program with 3 types: 10 \";"
 , "if ((x0) > (2)) {"
 , "    x1 = (x1) ++ (\"is more than 2\");"
 , "} else {"
 , "    x1 = (x1) ++ (\"is less than 2 or equals 2\");"
 , "}"
 , "x1"
 ]

log2 :: Script a => a (Int -> a Int)
log2 = function1 \a ->
  var (int (-1)) \result ->
  var (int 1)    \accum  ->
  sWhile (a @>= deref accum)
    ( accum  @= deref accum  @+ deref accum
    # result @= deref result @+ raw  1
    )
    #
  deref result

log2Result :: [String]
log2Result =
  [ "function f0(a) {"
  , "    var x1 = -1;"
  , "    var x2 = 1;"
  , "    while ((a) >= (x2)) {"
  , "        x2 = (x2) + (x2);"
  , "        x1 = (x1) + (1);"
  , "    }"
  , "    return (x1);"
  , "}"
  ]

fApplication :: Script a => a Int
fApplication = log2 @$$ raw 10

fApplicationResult :: [String]
fApplicationResult =
  [ "function f0(a) {"
  , "    var x1 = -1;"
  , "    var x2 = 1;"
  , "    while ((a) >= (x2)) {"
  , "        x2 = (x2) + (x2);"
  , "        x1 = (x1) + (1);"
  , "    }"
  , "    return (x1);"
  , "}"
  , "f0(10);"
  ]

twoArgFunction :: Script a => a (Int -> Int -> a Int)
twoArgFunction = function2 \a b ->
  var (int 0) \varA ->
  var (int 0) \varB ->
  varA @= a #
  varB @= b #
  sIf (a @> b)
    (varA @= a @* raw 2)
    (varB @= b @+ raw 4)
    #
  a @- b

twoArgFunctionResult :: [String]
twoArgFunctionResult =
  [ "function f0(a, b) {"
  , "    var x1 = 0;"
  , "    var x2 = 0;"
  , "    x1 = a;"
  , "    x2 = b;"
  , "    if ((a) > (b)) {"
  , "        x1 = (a) * (2);"
  , "    } else {"
  , "        x2 = (b) + (4);"
  , "    }"
  , "    return ((a) - (b));"
  , "}"
  ]

printTests :: TestTree
printTests = testGroup "script printer" $ map (uncurry testProperty)
  [ ("three types test",      testEquality threeTypes     threeTypesResult)
  , ("log2 test",             testEquality log2           log2Result)
  , ("log2 application test", testEquality fApplication   fApplicationResult)
  , ("two function test",     testEquality twoArgFunction twoArgFunctionResult)
  ]
