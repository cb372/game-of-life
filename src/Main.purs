module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS | e) Unit
main = do
  canvas <- getCanvasElementById "game"
  case canvas of
    Just c -> useCanvas c
    Nothing -> displayError
 
useCanvas :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
useCanvas canvas = do
  ctx <- getContext2D canvas
  _ <- beginPath ctx
  _ <- moveTo ctx 100.0 100.0
  _ <- lineTo ctx 200.0 200.0
  _ <- stroke ctx
  pure unit

displayError :: forall e. Eff (console :: CONSOLE | e) Unit
displayError = log "Looks like your browser doesn't support Canvas"
