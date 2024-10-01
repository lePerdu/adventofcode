import Control.Monad (forM_, when)
import Data.Array.Base (STUArray (STUArray))
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char (ord)
import Data.Foldable (find, traverse_)
import Data.List (intercalate, intersperse, nub)
import System.Exit (die)

data InputError = InvalidChar Char | InputNotGridShape deriving (Show)

type Coord = (Int, Int)

type Energy = Int

newtype OctoGrid = OctoGrid (UArray Coord Energy)

getAdjacent :: Coord -> [Coord]
getAdjacent (row, col) =
  [ (row, col + 1),
    (row - 1, col + 1),
    (row - 1, col),
    (row - 1, col - 1),
    (row, col - 1),
    (row + 1, col - 1),
    (row + 1, col),
    (row + 1, col + 1)
  ]

safeIndex :: (IArray a e, Ix i) => a i e -> i -> Maybe e
safeIndex arr ix = if inRange (bounds arr) ix then Just (arr ! ix) else Nothing

flashEnergy :: Int = 10

step :: OctoGrid -> OctoGrid
step (OctoGrid grid) = OctoGrid $ runSTUArray $ do
  -- Start off with every cell incremented
  grid <- newListArray (bounds grid) $ map (+ 1) $ elems grid
  -- Repeatedly find all > 9 and increment adjacent cells (except those with 0)
  cellsToFlash <-
    map fst . filter (\(_, energy) -> energy >= flashEnergy) <$> getAssocs grid
  flashStep grid cellsToFlash
  pure grid
  where
    updateFromFlash :: MArray a Energy m => a Coord Energy -> Coord -> m ()
    updateFromFlash grid adjacentCell = do
      adjEnergy <- readArray grid adjacentCell
      -- Don't increment already-flashed cells
      when (adjEnergy /= 0) $ writeArray grid adjacentCell (adjEnergy + 1)

    flashStep :: MArray a Energy m => a Coord Energy -> [Coord] -> m ()
    flashStep _ [] = pure ()
    flashStep grid (candidate : rest) = do
      current <- readArray grid candidate
      newCandidates <-
        if current >= flashEnergy
          then do
            writeArray grid candidate 0
            gridBounds <- getBounds grid
            let adjacent = filter (inRange gridBounds) $ getAdjacent candidate
            traverse_ (updateFromFlash grid) adjacent
            pure adjacent
          else pure []
      flashStep grid (rest ++ newCandidates)

countFlashes :: OctoGrid -> Int
countFlashes (OctoGrid grid) = length $ filter (== 0) $ elems grid

instance Show OctoGrid where
  show (OctoGrid grid) =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds grid
        showCell row col =
          let cellValue = grid ! (row, col)
           in if 0 <= cellValue && cellValue <= 9
                then show cellValue
                else show '#'
        showRow row = concatMap (showCell row) [minCol .. maxCol]
        rows = map showRow [minRow .. maxRow]
     in intercalate "\n" rows

parseEnergy :: Char -> Either InputError Int
parseEnergy c =
  if '0' <= c && c <= '9'
    then Right (ord c - ord '0')
    else Left (InvalidChar c)

parseGrid :: String -> Either InputError OctoGrid
parseGrid input = do
  heightRows <- traverse (traverse parseEnergy) (lines input)
  -- Pre-compute bounds to ensure the array will be well-defined
  maxBounds <- case nub $ map length heightRows of
    [singleCount] -> Right (length heightRows - 1, singleCount - 1)
    _ -> Left InputNotGridShape
  let rowsWithIndices = concatMap flattenRowIndex $ zip [0 ..] (map (zip [0 ..]) heightRows)
  pure $ OctoGrid $ array ((0, 0), maxBounds) rowsWithIndices
  where
    flattenRowIndex (row, colsWithIndices) =
      map (\(col, value) -> ((row, col), value)) colsWithIndices

readInput :: IO OctoGrid
readInput = do
  content <- getContents
  case parseGrid content of
    Right result -> pure result
    Left err -> die (show err)

stepCount :: Int = 100

getGridSize :: OctoGrid -> Int
getGridSize (OctoGrid grid) = rangeSize $ bounds grid

main :: IO ()
main = do
  grid <- readInput
  let steps = iterate step grid
  -- forM_ (zip [0 ..] steps) $ \(n, state) -> do
  --   putStrLn $ "After step " ++ show n
  --   print state
  --   putStrLn ""
  -- Exclude initial state when counting flashes
  let flashesPerStep = map countFlashes $ tail steps
  let totalFlashes = sum $ take stepCount flashesPerStep
  putStrLn $ "Part1: " ++ show totalFlashes
  let cellCount = getGridSize grid
  let Just (_, stepIndex) = find (\(flashCount, _) -> flashCount == cellCount) $ zip flashesPerStep [1 ..]
  putStrLn $ "Part2: " ++ show stepIndex
