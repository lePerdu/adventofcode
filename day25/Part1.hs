import Control.Monad (forM_)
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import System.Exit (die)

data Cell = Empty | East | South deriving (Eq, Ord, Enum)

instance Show Cell where
  show Empty = "."
  show East = ">"
  show South = "v"

type Coord = (Int, Int)

type Bounds = (Coord, Coord)

newtype Grid = Grid (UArray Coord Word8) deriving (Eq)

cellToWord :: Cell -> Word8
cellToWord = fromIntegral . fromEnum

wordToCell :: Word8 -> Cell
wordToCell = toEnum . fromIntegral

getRealCoord :: Grid -> Coord -> Coord
getRealCoord (Grid arr) (row, col) =
  let ((0, 0), (rMax, cMax)) = bounds arr
   in (row `mod` (rMax + 1), col `mod` (cMax + 1))

gridBounds :: Grid -> Bounds
gridBounds (Grid arr) = bounds arr

gridAt :: Grid -> Coord -> Cell
gridAt grid@(Grid arr) c =
  toEnum $ fromIntegral $ arr ! getRealCoord grid c

gridAssocs :: Grid -> [(Coord, Cell)]
gridAssocs (Grid arr) = [(c, wordToCell raw) | (c, raw) <- assocs arr]

applyAndCheck ::
  (Coord -> Cell -> Maybe Coord) ->
  [(Coord, Cell)] ->
  ([(Coord, Cell)], Bool)
applyAndCheck f = foldr foldFn ([], False)
  where
    foldFn orig@(coord, cell) (xs', applied) = case f coord cell of
      Just newCoord -> ((newCoord, cell) : xs', True)
      Nothing -> (orig : xs', applied)

mapCucAssocs :: (Coord -> Cell -> Maybe Coord) -> Grid -> Maybe Grid
mapCucAssocs getNewCoord grid =
  let (newAssocs, applied) = applyAndCheck getNewCoord $ filter ((/= Empty) . snd) (gridAssocs grid)
      withRealCoords =
        [(getRealCoord grid coord, cell) | (coord, cell) <- newAssocs]
   in if applied
        then
          Just $
            Grid $
              accumArray
                (\_ new -> cellToWord new)
                0
                (gridBounds grid)
                withRealCoords
        else Nothing

instance Show Grid where
  show grid =
    let ((minRow, minCol), (maxRow, maxCol)) = gridBounds grid
     in intercalate
          "\n"
          [ concatMap show [gridAt grid (row, col) | col <- [minCol .. maxCol]]
            | row <- [minRow .. maxRow]
          ]

advanceGrid :: Grid -> Maybe Grid
advanceGrid grid = case mapCucAssocs (aEast grid) grid of
  Just grid' -> Just $ fromMaybe grid' $ mapCucAssocs (aSouth grid') grid'
  Nothing -> mapCucAssocs (aSouth grid) grid
  where
    tryMove g new = if gridAt g new == Empty then Just new else Nothing

    aEast g orig@(row, col) East = tryMove g (row, col + 1)
    aEast _ c _ = Nothing

    aSouth g orig@(row, col) South = tryMove g (row + 1, col)
    aSouth _ c _ = Nothing

iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing f init = init : maybe [] (iterateUntilNothing f) (f init)

parseCell :: Char -> Maybe Cell
parseCell '.' = Just Empty
parseCell '>' = Just East
parseCell 'v' = Just South
parseCell _ = Nothing

parseGrid :: [String] -> Maybe Grid
parseGrid rows = do
  parsedRows <- traverse (traverse parseCell) rows
  let rowCount = length parsedRows
  let colCount = length $ head parsedRows
  pure $
    Grid $
      array ((0, 0), (rowCount - 1, colCount - 1)) $
        concat
          [ [ ((rowNum, colNum), cellToWord cell)
              | (colNum, cell) <- zip [0 ..] row
            ]
            | (rowNum, row) <- zip [0 ..] parsedRows
          ]

readInput :: IO Grid
readInput = do
  rows <- lines <$> getContents
  case parseGrid rows of
    Just g -> pure g
    Nothing -> die "Could not parse"

main :: IO ()
main = do
  grid <- readInput
  let steps = iterateUntilNothing advanceGrid grid
  forM_ (take 10 steps) $ \g -> print g >> putStrLn ""
  print $ length steps
