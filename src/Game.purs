module Game where

import Prelude ((*), (+), (==), ($), (<>))
import Data.Show
import Data.Sequence
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Data.Foldable (foldl)

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

setCell :: Board -> Coord -> Boolean -> Board
setCell (Board w h cells) coord value = 
  Board w h updatedCells where
    updatedCells = replace value (toIndex coord w) cells

toIndex :: Coord -> Width -> Int
toIndex coord (Width boardWidth) =
  (boardWidth * coord.y) + coord.x
