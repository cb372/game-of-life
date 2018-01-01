module Main where

import Prelude (Unit, bind, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, CanvasElement, getCanvasElementById, getContext2D)
import Game as G
import CanvasView (paintBoard)
import Signal (foldp, runSignal, (~>))
import Signal.Time (every, second)

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS | e) Unit
main = do
  canvas <- getCanvasElementById "game"
  case canvas of
    Just c -> run c
    Nothing -> displayError
 
initialBoard :: G.Board
initialBoard = G.setCell (G.mkEmptyBoard (G.Width 10) (G.Height 10)) { x: 3, y: 4} true

run :: forall e. CanvasElement -> Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
run canvas = do
  ctx <- getContext2D canvas
  runSignal $ (foldp (\_ -> G.iterate) initialBoard (every second)) ~> (paintBoard ctx)

displayError :: forall e. Eff (console :: CONSOLE | e) Unit
displayError = log "Looks like your browser doesn't support Canvas"
