-------------------------------------------------------------
-- |
-- Module      : ScriptPrinter
-- Description : JavaScript-like eDSL AST printer.
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
-- Language    : Haskell2010
--
-- JavaScript-like eDSL abstract syntax tree pretty printer.
-------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies   #-}

module ScriptPrinter
  ( Printer (..)
  , view
  ) where

import Data.Functor.Const (Const (..))

import Script (Script (..))


-- | This newtype is used to print an abstract syntax tree of 'Script' embedded
-- domain specific language.
newtype Printer a = Printer { unPrinter :: Int -> String }

printerOperator :: String -> Printer a -> Printer b -> Printer c
printerOperator str a b = Printer \n ->
  "(" <> unPrinter a n <> ") " <> str <> " (" <> unPrinter b n <> ")"

indent :: String -> String
indent = unlines . map ("    " <>) . lines

withReturn :: Int -> Printer a -> String
withReturn n a =
  let (x:xs) = reverse . lines $ unPrinter a (n + 1)
  in (indent . unlines $ reverse xs)
  <> "    return ("
  <> x
  <> ");\n"

-- | To lanch 'Printer'.
view :: Printer a -> String
view = flip unPrinter 0

instance Script Printer where
  type Var Printer = Const String

  raw x = Printer . const $ show x
  new x = Printer . const $ show x

  deref val = Printer . const $ getConst val

  a @= b = Printer \n -> getConst a <> " = " <> unPrinter b n <> ";"
  a #  b = Printer \n -> unPrinter a n <> "\n" <> unPrinter b n
  f @$ a =
    Printer (unPrinter f) #
    Printer \n -> "f" <> show n <> "(" <> unPrinter a n <> ");"
  f @$$ a =
    Printer (unPrinter f) #
    Printer \n -> "f" <> show n <> "(" <> unPrinter a n <> ");"

  var value usage = Printer \n ->
    let x = "x" <> show n
    in "var "
    <> x
    <> " = "
    <> unPrinter value n
    <> ";\n"
    <> unPrinter (usage $ Const x) (n + 1)

  function1 f = Printer \n ->
    "function f"
    <> show n
    <> "(a) {\n"
    <> (withReturn n . f . Printer $ const "a")
    <> "}"

  function2 f = Printer \n ->
    "function f"
    <> show n
    <> "(a, b) {\n"
    <> withReturn n (f (Printer $ const "a") (Printer $ const "b"))
    <> "}"

  sIf cond onTrue onFalse = Printer \n ->
    "if ("
    <> unPrinter cond n
    <> ") {\n"
    <> indent (unPrinter onTrue n)
    <> "} else {\n"
    <> indent (unPrinter onFalse n)
    <> "}"

  sWhile cond body = Printer \n ->
    "while ("
    <> unPrinter cond n
    <> ") {\n"
    <> indent (unPrinter body n)
    <> "}"

  (@==)  = printerOperator "=="
  (@!=)  = printerOperator "/="
  (@>)   = printerOperator ">"
  (@<)   = printerOperator "<"
  (@>=)  = printerOperator ">="
  (@<=)  = printerOperator "<="
  (@+)   = printerOperator "+"
  (@-)   = printerOperator "-"
  (@*)   = printerOperator "*"
  (@&&)  = printerOperator "&&"
  (@||)  = printerOperator "||"
  (@++)  = printerOperator "++"
