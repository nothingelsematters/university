module Generator
  ( intGenerator
  , intListGenerator
  , listGenerator
  , randomChar
  , randomList
  , randomString
  , stringGenerator
  ) where

import Hedgehog (Gen, MonadGen, PropertyT, Range, forAll)
import qualified Hedgehog.Gen as Gen (alphaNum, int, list, string)
import qualified Hedgehog.Range as Range (constant)


standardRange :: Range Int
standardRange = Range.constant 0 100

stringGenerator :: Gen String
stringGenerator = Gen.string (Range.constant 1 1000) Gen.alphaNum

randomString :: Monad m => PropertyT m String
randomString = forAll stringGenerator

randomChar :: Monad m => PropertyT m Char
randomChar = forAll Gen.alphaNum

intGenerator :: MonadGen m => m Int
intGenerator = Gen.int standardRange

listGenerator :: MonadGen m => m a -> m [a]
listGenerator = Gen.list standardRange

intListGenerator :: MonadGen m => m [Int]
intListGenerator = listGenerator intGenerator

randomList :: Monad m => PropertyT m [Int]
randomList = forAll $ listGenerator intGenerator
