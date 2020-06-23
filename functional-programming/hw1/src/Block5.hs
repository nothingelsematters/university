-- | The fifth block of the first home work is dedicated to monads and monad
-- connected computations.
module Block5
  ( ArithmeticError(..)
  , Expression(..)
  , eval
  , moving
  ) where

import Control.Monad.State.Lazy


-- | A mathematical expression data structure.
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^:
data Expression
  = Constant Int
  | Expression :+: Expression
  | Expression :-: Expression
  | Expression :*: Expression
  | Expression :/: Expression
  | Expression :^: Expression
  deriving Show

-- | A newtype expressing arithmetic error with error messgae during
-- computations.
newtype ArithmeticError = ArithmeticError String deriving (Eq, Show)

binaryOperation
  :: (Int -> Either ArithmeticError Int)
  -> (Int -> Int -> Int)
  -> Expression
  -> Expression
  -> Either ArithmeticError Int
binaryOperation except op left right =
  liftM2 op (eval left) (eval right >>= except)

safeBinaryOperation
  :: (Int -> Int -> Int)
  -> Expression
  -> Expression
  -> Either ArithmeticError Int
safeBinaryOperation = binaryOperation pure

-- | Evaluates a given 'Expression', returns 'Left' 'ArithmeticError' on failure
-- and 'Right' 'Int' on success.
eval :: Expression -> Either ArithmeticError Int
eval (Constant c) = Right c
eval (a :+: b)    = safeBinaryOperation (+) a b
eval (a :-: b)    = safeBinaryOperation (-) a b
eval (a :*: b)    = safeBinaryOperation (*) a b
eval (a :/: b)    = binaryOperation divisionByZeroCheck div a b
  where
    divisionByZeroCheck :: Int -> Either ArithmeticError Int
    divisionByZeroCheck x =
      if x == 0
      then Left $ ArithmeticError "Division by zero"
      else pure x
eval (a :^: b)    = binaryOperation negativePowerCheck (^) a b
  where
    negativePowerCheck :: Int -> Either ArithmeticError Int
    negativePowerCheck x =
      if x < 0
      then Left $ ArithmeticError "Negative power"
      else pure x


data StepProperty a = StepProperty [a] [a] a Int

base :: Fractional a => State (StepProperty a) a
base = do
  (StepProperty xs ys current frame) <- get
  let newCurrent = current + head ys
  let newFrame = frame + 1
  put $ StepProperty xs (tail ys) newCurrent newFrame
  return $ newCurrent / realToFrac newFrame

step :: Fractional a => State (StepProperty a) a
step = do
  (StepProperty xs ys current frame) <- get
  let newCurrent = current + head ys - head xs
  put $ StepProperty (tail xs) (tail ys) newCurrent frame
  return $ newCurrent / realToFrac frame

-- | A Simple Moving Average algorithm implementation using 'State' monad.
moving :: Fractional a => Int -> [a] -> [a]
moving frame xs = evalState
  (sequence . take xsSize $ replicate frame base ++
  replicate (xsSize - frame) step) $
  StepProperty xs xs 0 0
    where
      xsSize :: Int
      xsSize = length xs
