module CanvasView where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Traversable
import Data.Sequence
import Data.Tuple
import Data.Int
import Graphics.Canvas
import Game

-- TODO make canvas size configurable?
canvasWidth :: Int
canvasWidth = 400

canvasHeight :: Int
canvasHeight = 400

paintBoard :: forall e. Context2D -> Board -> Eff (canvas :: CANVAS | e) Unit
paintBoard ctx (b @ Board w _ cells) = do
  _ <- traverse (\(Tuple coord cell) -> paintCell ctx b coord cell) cellsWithCoords 
  pure unit 
  where
    cellsWithCoords :: Seq (Tuple Coord Boolean)
    cellsWithCoords = mapWithIndex (\i c -> Tuple (toCoord i w) c) cells

paintCell :: forall e. Context2D -> Board -> Coord -> Boolean -> Eff (canvas :: CANVAS | e) Unit
paintCell ctx board coord true = do
  _ <- fillRect ctx (coordToRectangle board coord)
  pure unit
paintCell ctx board coord false = do
  _ <- strokeRect ctx (coordToRectangle board coord)
  pure unit

coordToRectangle :: Board -> Coord -> Rectangle
coordToRectangle (Board (Width w) (Height h) _) coord =
  {
    x: toNumber $ coord.x * (canvasWidth / w),
    y: toNumber $ coord.y * (canvasHeight / h),
    w: toNumber $ canvasWidth / w,
    h: toNumber $ canvasHeight / h
  }
