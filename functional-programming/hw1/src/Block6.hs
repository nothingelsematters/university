{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The sixth block of the first home work is dedicated to parser-combinators.
module Block6
  ( Parser(..)
  , element
  , eof
  , integerParser
  , listlistParser
  , ok
  , parenthesesSequenceParser
  , satisfy
  , stream
  ) where

import Control.Applicative (Alternative, empty, many, optional, some, (<|>))
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Function (on)


-- | A simple parser type, that stores a function, that being given a list of
-- values returns 'Nothing' on failure and 'Just' with value parsed and
-- remaining part of a list on success.
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  f `fmap` Parser parser = Parser $ fmap (first f) . parser

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ pure . (,) a

  (<*>) :: forall t a b . Parser t (a -> b) -> Parser t a -> Parser t b
  Parser f <*> Parser a = Parser $ (>>= applySecond) . f
    where
      applySecond :: (a -> b, [t]) -> Maybe (b, [t])
      applySecond (res, l) = first res <$> a l

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser a >>= f = Parser $ \x -> a x >>= uncurry (runParser . f)

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser a <|> Parser b = Parser $ \s -> on (<|>) ($ s) a b

-- | Parser that never fails and doesn't modify input.
ok :: Parser s ()
ok = pure ()

-- | Checks whether a parser has no more input.
eof :: Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- | Checks whether an element satisfies a given predicate and process it on
-- success.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
  []     -> Nothing
  (x:xs) -> if p x then Just (x, xs) else Nothing

-- | Parser with a certain element to process.
element :: Eq s => s -> Parser s s
element = satisfy . (==)

-- | Parser with a given stream of elements to process.
stream :: (Eq s, Traversable t) => t s -> Parser s (t s)
stream = traverse element

-- | Parentheses sequence parser.
parenthesesSequenceParser :: Parser Char ()
parenthesesSequenceParser = parser >> eof
  where
    parser :: Parser Char [String]
    parser = many $ (stream "(" >> parser >> stream ")") <|> (stream "()")

-- | Integer parser with optional "+" and "-" prefix.
integerParser :: Parser Char Int
integerParser = do
  prefix <- (\x -> if x == Just '-' then negate else id) <$>
    optional (element '-' <|> element '+')
  prefix . read <$> some (satisfy isDigit)

skippingSpaces :: Parser Char a -> Parser Char a
skippingSpaces = (many (satisfy isSpace) >>)

listParser :: Parser Char a -> Parser Char [a]
listParser elementParser = do
  size <- skippingSpaces integerParser
  sequenceA . replicate size $ do
    _ <- skippingSpaces $ element ','
    skippingSpaces elementParser

-- | A list of lists of integers parser. It relies on list size being its first
-- element.
listlistParser :: Parser Char [[Int]]
listlistParser = do
  let integerListParser = listParser integerParser
  firstList <- integerListParser
  (firstList :) <$> many (skippingSpaces (element ',') >> integerListParser)
