module Game where

import Prelude ((*), (+), (==), ($), (<>), (-), (<), (>=), (||), mod, div, id)
import Data.Show (class Show)
import Data.Maybe
import Data.Tuple (Tuple(..), fst)
import Data.Sequence (Seq, cons, empty, index, length, replace, snoc, splitAt)
import Data.Unfoldable (replicate)
import Data.Foldable (foldl)
import Data.Array as A

newtype Width = Width Int
newtype Height = Height Int
data Board = Board Width Height (Seq Boolean)

instance showBoard :: Show Board where
  show board = 
    foldl (\acc row -> acc <> (showRow row) <> "\n") "" rs
    where
      rs = rows board

showCell :: Boolean -> String
showCell true = "x"
showCell false = "-"

showRow :: (Seq Boolean) -> String
showRow xs = foldl (\acc x -> acc <> (showCell x) <> " ") "" xs 
rows :: Board -> Seq (Seq Boolean)
rows (Board (Width w) height cells) = 
  chunks w cells where
    chunks :: forall a. Int -> Seq a -> Seq (Seq a)
    chunks _ xs | length xs == 0 = empty
    chunks n xs =
      let Tuple ys zs = splitAt n xs
      in cons ys (chunks n zs)

type Coord = {
  x :: Int,
  y :: Int
}

mkEmptyBoard :: Width -> Height -> Board
mkEmptyBoard (_w @ Width w) (_h @ Height h) = Board _w _h (replicate (w * h) false)

getCell :: Board -> Coord -> Maybe Boolean
getCell (Board (w_ @ Width w) (Height h) cells) c =
  if c.x < 0 || c.x >= w || c.y < 0 || c.y >= h
    then Nothing
    else index (toIndex c w_) cells

setCell :: Board -> Coord -> Boolean -> Board
setCell (Board w h cells) coord value = 
  Board w h updatedCells where
    updatedCells = replace value (toIndex coord w) cells

-- TODO bounds check here, return Maybe Int?
toIndex :: Coord -> Width -> Int
toIndex coord (Width boardWidth) =
  (boardWidth * coord.y) + coord.x

toCoord :: Int -> Width -> Coord
toCoord i (Width boardWidth) = 
  { x: i `mod` boardWidth, y: i `div` boardWidth }

{-
Apply the following rules to produce the next generation of the board.

Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
Any live cell with two or three live neighbours lives on to the next generation.
Any live cell with more than three live neighbours dies, as if by overpopulation.
Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
-}
iterate :: Board -> Board
iterate (b @ Board w h cells) =
  Board w h iterateCells where 
    iterateCells :: Seq Boolean
    iterateCells = mapWithIndex (applyRules b) cells
  
applyRules :: Board -> Int -> Boolean -> Boolean
applyRules (b @ Board w _ _) i cell =
  let n = countLiveNeighbours b (toCoord i w) in
  case Tuple cell n of
    Tuple false 3 -> true
    Tuple true 2 -> true
    Tuple true 3 -> true
    Tuple true _ -> false
    Tuple false _ -> false
  
countLiveNeighbours :: Board -> Coord -> Int
countLiveNeighbours (b @ Board (Width w) _ cells) c = 
  A.length $ A.filter id $ A.mapMaybe (getCell b) neighbours where
    neighbours = [
      { x: c.x - 1, y: c.y - 1 },
      { x: c.x    , y: c.y - 1 },
      { x: c.x + 1, y: c.y - 1 },
      { x: c.x - 1, y: c.y     },
      { x: c.x + 1, y: c.y     },
      { x: c.x - 1, y: c.y + 1 },
      { x: c.x    , y: c.y + 1 },
      { x: c.x + 1, y: c.y + 1 }
    ]
  
mapWithIndex :: forall a b. (Int -> a -> b) -> Seq a -> Seq b
mapWithIndex f xs = fst $ foldl g z xs where

  g :: (Tuple (Seq b) Int) -> a -> (Tuple (Seq b) Int)
  g (Tuple acc i) x = Tuple (snoc acc (f i x)) (i + 1)

  z = Tuple empty 0
