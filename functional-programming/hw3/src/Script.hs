---------------------------------------------------------
-- |
-- Module      : Script
-- Description : yet another JavaScript-like eDSL
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
-- Language    : Haskell2010
--
-- JavaScript-like eDSL with variables and more support.
---------------------------------------------------------
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Script
  ( Interpreter (..)
  , Script (..)
  , Value
  , eval
  ) where

import Control.Monad (join, liftM2)
import Control.Monad.Extra (ifM)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)


-- | Dummy class that stricts 'Script' eDSL to certain types.
class Show v => Value v

instance Value Int
instance Value Bool
instance Value String

-- | Script monad is a tagelss final embedded domain specific language. It is
-- like a JavaScript subset, but with static types and other minor differences.
-- It is advised to use @BlockArguments@ extension for a little bit more
-- friendly 'var' (new variable initialization), 'function1' and 'function2'
-- (function definiton) interfaces.
class Script repr where
  type Var repr :: * -> *

  new :: Value a => a -> repr (Var repr a)

  int :: Int -> repr (Var repr Int)
  int = new

  bool :: Bool -> repr (Var repr Bool)
  bool = new

  string :: String -> repr (Var repr String)
  string = new

  deref :: Value a => Var repr a -> repr a

  raw :: Value a => a -> repr a
  default raw :: Applicative repr => Value a => a -> repr a
  raw = pure

  infixl 1 #
  (#) :: repr a -> repr b -> repr b
  default (#) :: Monad repr => repr a -> repr b -> repr b
  (#) = (>>)

  var :: Value a => repr (Var repr a) -> (Var repr a -> repr b) -> repr b
  default var
    :: (Monad repr, Value a)
    => repr (Var repr a)
    -> (Var repr a -> repr b)
    -> repr b
  var = (>>=)

  function1 :: (repr a -> repr b) -> repr (a -> repr b)
  default function1
    :: Applicative repr
    => (repr a -> repr b)
    -> repr (a -> repr b)
  function1 f = pure $ f . pure

  function2 :: (repr a -> repr b -> repr c) -> repr (a -> b -> repr c)
  default function2
    :: Applicative repr
    => (repr a -> repr b -> repr c)
    -> repr (a -> b -> repr c)
  function2 f = pure \a b -> f (pure a) (pure b)

  infixr 0 @$
  (@$) :: repr (a -> b) -> repr a -> repr b

  infixr 0 @$$
  (@$$) :: repr (a -> repr b) -> repr a -> repr b

  sWhile :: repr Bool -> repr () -> repr ()
  default sWhile :: Monad repr => repr Bool -> repr () -> repr ()
  sWhile = whileM_

  sIf :: repr Bool -> repr () -> repr () -> repr ()
  default sIf :: Monad repr => repr Bool -> repr () -> repr () -> repr ()
  sIf = ifM

  infixr 4 @:=
  (@:=) :: Value a => Var repr a -> a -> repr ()
  variable @:= value = variable @= raw value

  infixr 4 @=
  (@=) :: Value a => Var repr a -> repr a -> repr ()
  default (@=) :: (Monad repr, Value a) => Var repr a -> repr a -> repr ()
  varb @= val = val >>= (varb @:=)

  infix 4 @==
  (@==) :: (Eq a, Value a) => repr a -> repr a -> repr Bool

  infix 4 @!=
  (@!=) :: (Eq a, Value a) => repr a -> repr a -> repr Bool

  infix 4 @>
  (@>) :: (Ord a, Value a) => repr a -> repr a -> repr Bool

  infix 4 @<
  (@<) :: (Ord a, Value a) => repr a -> repr a -> repr Bool

  infix 4 @>=
  (@>=) :: (Ord a, Value a) => repr a -> repr a -> repr Bool

  infix 4 @<=
  (@<=) :: (Ord a, Value a) => repr a -> repr a -> repr Bool

  infixl 6 @+
  (@+) :: (Num a, Value a) => repr a -> repr a -> repr a

  infixl 6 @-
  (@-) :: (Num a, Value a) => repr a -> repr a -> repr a

  infixl 7 @*
  (@*) :: (Num a, Value a) => repr a -> repr a -> repr a

  infixr 3 @&&
  (@&&) :: repr Bool -> repr Bool -> repr Bool

  infixr 2 @||
  (@||) :: repr Bool -> repr Bool -> repr Bool

  infixr 5 @++
  (@++) :: (Value [a]) => repr [a] -> repr [a] -> repr [a]


-- | Interpreter for 'Script' eDSL.
newtype Interpreter s a
  = Interpreter { compile :: ST s a }
  deriving (Functor, Applicative, Monad)

-- | To eval an interpreted via 'Interpreter' 'Script' expression.
eval :: (forall s . Interpreter s a) -> a
eval x = runST $ compile x

instance Script (Interpreter s) where
  type Var (Interpreter s) = STRef s
  new     = Interpreter . newSTRef
  deref   = Interpreter . readSTRef
  a @:= b = Interpreter $ writeSTRef a b
  (@$)    = (<*>)
  f @$$ a = join $ f <*> a

  (@==)  = liftM2 (==)
  (@!=)  = liftM2 (/=)
  (@>)   = liftM2 (>)
  (@<)   = liftM2 (<)
  (@>=)  = liftM2 (>=)
  (@<=)  = liftM2 (<=)
  (@+)   = liftM2 (+)
  (@-)   = liftM2 (-)
  (@*)   = liftM2 (*)
  (@&&)  = liftM2 (&&)
  (@||)  = liftM2 (||)
  (@++)  = liftM2 (++)
