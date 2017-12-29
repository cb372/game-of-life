module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Game
import CanvasView

{-
TODO
implement CanvasDisplay
implement UI to initialise cells and start iterations
ref to Game
setInterval to iterate game state
-}
main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS | e) Unit
main = do
  canvas <- getCanvasElementById "game"
  case canvas of
    Just c -> useCanvas c
    Nothing -> displayError
 
board :: Board
board = setCell (mkEmptyBoard (Width 10) (Height 10)) { x: 3, y: 4} true

useCanvas :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
useCanvas canvas = do
  ctx <- getContext2D canvas
  paintBoard ctx board

displayError :: forall e. Eff (console :: CONSOLE | e) Unit
displayError = log "Looks like your browser doesn't support Canvas"
