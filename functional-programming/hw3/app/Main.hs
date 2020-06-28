{-# LANGUAGE BlockArguments #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import Data.Function (on)
import System.Random (RandomGen)
import UI.NCurses (Curses, Event (..), Window, clear, defaultWindow, drawBox, drawString, getEvent,
                   moveCursor, newWindow, render, runCurses, setEcho, updateWindow)

import Comonad19 (Plague, explore, getDefaultConfig, plague, step)

side :: Num a => a
side = 20

main :: IO ()
main = do
  config <- getDefaultConfig
  flip (either putStrLn) (plague config) \virus ->
    runCurses do
      setEcho False
      _   <- flip updateWindow clear <$> defaultWindow
      let boxSide = side * 2 + 3
      box <- newWindow (boxSide + 2) boxSide 0 0
      updateWindow box do
        moveCursor boxSide 1
        drawString "press q to exit"
      spread box virus

spread :: RandomGen r => Window -> Plague r -> Curses ()
spread w virus = do
  updateWindow w do
    moveCursor 1 0
    drawString . unlines . map (" " <>) . lines $ explore side virus
    drawBox Nothing Nothing
  render
  ev <- getEvent w (Just 500)
  flip when (spread w $ step virus) $
    on (&&) ((ev /=) . pure . EventCharacter) 'q' 'Q'
