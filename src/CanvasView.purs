module CanvasView where

import Prelude (Unit, bind, pure, unit, ($), (*), (/))
import Control.Monad.Eff (Eff)
import Data.Traversable (traverse)
import Data.Sequence (Seq)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Graphics.Canvas (CANVAS, Context2D, Rectangle, fillRect, strokeRect, setFillStyle)
import Game (Board(..), Coord, Height(..), Width(..), mapWithIndex, toCoord)

-- TODO make canvas size configurable?
canvasWidth :: Int
canvasWidth = 680

canvasHeight :: Int
canvasHeight = 680

paintBoard :: forall e. Context2D -> Board -> Eff (canvas :: CANVAS | e) Unit
paintBoard ctx (b @ Board w _ cells) = do
  _ <- traverse (\(Tuple coord cell) -> paintCell ctx b coord cell) cellsWithCoords 
  pure unit 
  where
    cellsWithCoords :: Seq (Tuple Coord Boolean)
    cellsWithCoords = mapWithIndex (\i c -> Tuple (toCoord i w) c) cells

paintCell :: forall e. Context2D -> Board -> Coord -> Boolean -> Eff (canvas :: CANVAS | e) Unit
paintCell ctx board coord true = do
  _ <- setFillStyle "#000000" ctx
  _ <- fillRect ctx (coordToRectangle board coord)
  pure unit
paintCell ctx board coord false = do
  _ <- setFillStyle "#ffffff" ctx
  _ <- fillRect ctx (coordToRectangle board coord)
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
