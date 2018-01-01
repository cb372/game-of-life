module Main where

import Prelude (Unit, bind, ($), (<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Graphics.Canvas (CANVAS, CanvasElement, getCanvasElementById, getContext2D)
import Game
import CanvasView (paintBoard)
import Signal (foldp, runSignal, (~>))
import Signal.Time (every, second)

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS | e) Unit
main = do
  canvas <- getCanvasElementById "game"
  case canvas of
    Just c -> run c
    Nothing -> displayError
 
run :: forall e. CanvasElement -> Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
run canvas = do
  ctx <- getContext2D canvas
  _ <- log $ show initialBoard
  runSignal $ (foldp (\_ -> iterate) initialBoard (every second)) ~> (paintBoard ctx)

initialBoard :: Board
initialBoard = 
  setCell' { x: 4,  y: 2  } <<<
  setCell' { x: 5,  y: 2  } <<<
  setCell' { x: 6,  y: 2  } <<<
  setCell' { x: 10, y: 2  } <<<
  setCell' { x: 11, y: 2  } <<<
  setCell' { x: 12, y: 2  } <<<
  setCell' { x: 2,  y: 4  } <<<
  setCell' { x: 7,  y: 4  } <<<
  setCell' { x: 9,  y: 4  } <<<
  setCell' { x: 14, y: 4  } <<<
  setCell' { x: 2,  y: 5  } <<<
  setCell' { x: 7,  y: 5  } <<<
  setCell' { x: 9,  y: 5  } <<<
  setCell' { x: 14, y: 5  } <<<
  setCell' { x: 2,  y: 6  } <<<
  setCell' { x: 7,  y: 6  } <<<
  setCell' { x: 9,  y: 6  } <<<
  setCell' { x: 14, y: 6  } <<<
  setCell' { x: 4,  y: 7  } <<<
  setCell' { x: 5,  y: 7  } <<<
  setCell' { x: 6,  y: 7  } <<<
  setCell' { x: 10, y: 7  } <<<
  setCell' { x: 11, y: 7  } <<<
  setCell' { x: 12, y: 7  } <<<
  setCell' { x: 4,  y: 9  } <<<
  setCell' { x: 5,  y: 9  } <<<
  setCell' { x: 6,  y: 9  } <<<
  setCell' { x: 10, y: 9  } <<<
  setCell' { x: 11, y: 9  } <<<
  setCell' { x: 12, y: 9  } <<<
  setCell' { x: 2,  y: 10 } <<<
  setCell' { x: 7,  y: 10 } <<<
  setCell' { x: 9,  y: 10 } <<<
  setCell' { x: 14, y: 10 } <<<
  setCell' { x: 2,  y: 11 } <<<
  setCell' { x: 7,  y: 11 } <<<
  setCell' { x: 9,  y: 11 } <<<
  setCell' { x: 14, y: 11 } <<<
  setCell' { x: 2,  y: 12 } <<<
  setCell' { x: 7,  y: 12 } <<<
  setCell' { x: 9,  y: 12 } <<<
  setCell' { x: 14, y: 12 } <<<
  setCell' { x: 4,  y: 14 } <<<
  setCell' { x: 5,  y: 14 } <<<
  setCell' { x: 6,  y: 14 } <<<
  setCell' { x: 10, y: 14 } <<<
  setCell' { x: 11, y: 14 } <<<
  setCell' { x: 12, y: 14 }
    $ emptyBoard
  where
    setCell' coord board = setCell board coord true
    emptyBoard = mkEmptyBoard (Width 17) (Height 17)
  
displayError :: forall e. Eff (console :: CONSOLE | e) Unit
displayError = log "Looks like your browser doesn't support Canvas"
