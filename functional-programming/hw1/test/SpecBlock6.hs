{-# LANGUAGE OverloadedStrings #-}

module SpecBlock6
  ( sixthBlockTest
  ) where

import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)

import Block6
import Generator (intGenerator, listGenerator, randomChar, randomString)
import InstanceTest (simpleTests)

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen (element, list)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)


infixl 5 @>
(@>) :: Parser s a -> [s] -> Maybe (a, [s])
(@>) = runParser

infixl 3 ?>
(?>) :: Parser Char Int -> Parser Char Int -> Bool
(?>) = on (==) $ \x ->
  map ($ x) $ map (flip (@>) . (show :: Int -> String)) [0..10]

parserInstancesTests :: TestTree
parserInstancesTests = simpleTests "parser instances tests"
  [ ( "rule \"fmap id  ==  id\"", fmap id integerParser ?> id integerParser)
  , ( "rule \"fmap (f . g)  ==  fmap f . fmap g\""
    , fmap (f . g) integerParser  ?>  (fmap f . fmap g) integerParser
    )
  , ( "rule \"pure f <*> pure x = pure (f x)\"", pure f <*> w ?> pure (f 0))
  , ( "rule \"u <*> pure y = pure ($ y) <*> u\""
    , (pure f) <*> w ?> pure ($ 0) <*> (pure f)
    )
  , ( "rule \"pure id <*> v = v\"", pure id <*> w ?> w)
  , ( "rule \"pure (.) <*> u <*> v <*> w = u <*> (v <*> w)\""
    , pure (.) <*> (pure f) <*> (pure g) <*> w ?> (pure f) <*> ((pure g) <*> w)
    )
  , ( "rule \"return a >>= k = k a\""
    , (return x >>= (return . f)) ?> (return . f) x
    )
  , ( "rule \"m >>= return = m\"", (w >>= return) ?> w)
  , ( "rule \"m >>= (\\x -> k x >>= h) = (m >>= k) >>= h\""
    , (w >>= (\y -> (return . f) y >>= (return . g))) ?>
        ((w >>= (return . f)) >>= (return . g))
    )
  ]
    where
      f :: Int -> Int
      f = (+ 1)

      g :: Int -> Int
      g = (* 2)

      x :: Int
      x = 0

      w :: Parser Char Int
      w = pure 0


okTest :: Property
okTest = property $ do
  str <- randomString
  ok @> str === Just ((), str)

eofFailTest :: Property
eofFailTest = property $ do
  str <- randomString
  eof @> str === Nothing

eofSuccessTest :: Property
eofSuccessTest = property $ do
  eof @> "" === Just ((), "")

elementSuccessTest :: Property
elementSuccessTest = property $ do
  (h:str) <- randomString
  element h @> (h:str) === Just (h, str)

elementRandomTest :: Property
elementRandomTest = property $ do
  (h:str) <- randomString
  ch <- randomChar
  (element ch @> (h:str) ===) $
    if ch == h
    then Just (h, str)
    else Nothing

streamSuccessTest :: Property
streamSuccessTest = property $ do
  str <- randomString
  let substring = take (length str `div` 2) str
  let rest = drop (length str `div` 2) str
  stream substring @> str === Just (substring, rest)

simpleParsersTest :: TestTree
simpleParsersTest = testGroup "simple parser tests" .
  map (uncurry testProperty) $
    [ ("ok parser test", okTest)
    , ("eof fail parser test", eofFailTest)
    , ("eof success parser test", eofSuccessTest)
    , ("element (and satisfy) success parser test", elementSuccessTest)
    , ("element (and satisfy) random parser test", elementRandomTest)
    , ("stream success test", streamSuccessTest)
    ]

parenthesesSequenceParserTests :: TestTree
parenthesesSequenceParserTests = simpleTests "parentheses sequence parser tests"
  [ ( "empty string",  isJust $ parenthesesSequenceParser @> "")
  , ( "two parentheses",  isJust $  parenthesesSequenceParser @> "()")
  , ( "simple wrong",  isNothing $ parenthesesSequenceParser @> "(")
  , ( "complicated success"
    , isJust $ parenthesesSequenceParser @> "((()()()())()()())()()(())"
    )
  , ( "complicated failure"
    , isNothing $ parenthesesSequenceParser @> "((()()()())()(())()()(())"
    )
  ]

integerParserTests :: TestTree
integerParserTests = simpleTests "integer parser failure tests"
  [ ("simple failure", isNothing $ integerParser @> "")
  , ("simple character failure", isNothing $ integerParser @> "!")
  , ("no number failure", isNothing $ integerParser @> "+")
  ]

listsListParserTests :: TestTree
listsListParserTests =
  simpleTests "list of lists of integers parser failure tests"
    [ ("size is wrong", isNothing $ listlistParser @> "1")
    , ("no coma, omfg", isNothing $ listlistParser @> "1 1, 1, 1")
    ]

integerParserSuccessTests :: TestTree
integerParserSuccessTests = testProperty "random integer parser tests" .
  property $ do
    prefix <- forAll $ Gen.element ["+", "-", ""]
    let modification = if prefix == "-" then negate else id
    number <- forAll intGenerator
    integerParser @> (prefix ++ show number) === Just (modification number, "")

listlistPareserSuccessTests :: TestTree
listlistPareserSuccessTests = testProperty "random lists list pareser test" .
  property $ do
    argument <- forAll . Gen.list (Range.constant 1 5) . listGenerator $
      intGenerator
    let comas = intercalate "," . map (intercalate "," . map show)
    let str = comas . map (\xs -> length xs:xs) $ argument
    listlistParser @> str === Just (argument, "")

sixthBlockTest :: TestTree
sixthBlockTest = testGroup "Sixth block tests"
    [ parserInstancesTests
    , simpleParsersTest
    , parenthesesSequenceParserTests
    , integerParserTests
    , listsListParserTests
    , integerParserSuccessTests
    , listlistPareserSuccessTests
    ]
