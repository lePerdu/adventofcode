import Data.Array.Unboxed
import Data.Char
import Data.List
import Data.Set qualified as Set
import System.Exit (die)

data InputError = InvalidChar Char | InputNotGridShape deriving (Show)

type Coord = (Int, Int)

type Grid = UArray Coord Int

getAdjacent :: Coord -> [Coord]
getAdjacent (row, col) =
  [ (row + 1, col),
    (row - 1, col),
    (row, col + 1),
    (row, col - 1)
  ]

safeIndex :: (IArray a e, Ix i) => a i e -> i -> Maybe e
safeIndex arr ix = if inRange (bounds arr) ix then Just (arr ! ix) else Nothing

isLowPoint :: Coord -> Grid -> Bool
isLowPoint c grid
  | inRange (bounds grid) c =
      let cellHeight = grid ! c
          -- Less than cell height or off the grid
          lessThanCellHeight adj =
            maybe True (> cellHeight) (safeIndex grid adj)
       in all lessThanCellHeight (getAdjacent c)
isLowPoint _ _ = False -- If OOB

findLowPoints :: Grid -> [Coord]
findLowPoints grid = filter (`isLowPoint` grid) (indices grid)

getRisk :: Grid -> Coord -> Int
getRisk grid coord = maybe 0 (+ 1) (safeIndex grid coord)

computeGridRisk :: Grid -> [Coord] -> Int
computeGridRisk grid coords = sum $ map (getRisk grid) coords

higherAdjacent :: Grid -> Coord -> [Coord]
higherAdjacent grid c
  | inRange (bounds grid) c =
      let height = grid ! c
          above adj =
            maybe False (\h -> height <= h && h < 9) (safeIndex grid adj)
       in filter above (getAdjacent c)
higherAdjacent _ _ = []

expandBasin :: Grid -> Coord -> Set.Set Coord
expandBasin grid lowPoint = dfs Set.empty [lowPoint]
  where
    dfs visited queue = case queue of
      [] -> visited
      (nextCoord : rest) ->
        if Set.member nextCoord visited
          then dfs visited rest
          else
            let expandedPoints = higherAdjacent grid nextCoord
             in dfs (Set.insert nextCoord visited) (rest ++ expandedPoints)

parseHeight :: Char -> Either InputError Int
parseHeight c =
  if '0' <= c && c <= '9'
    then Right (ord c - ord '0')
    else Left (InvalidChar c)

parseGrid :: String -> Either InputError Grid
parseGrid input = do
  heightRows <- traverse (traverse parseHeight) (lines input)
  -- Pre-compute bounds to ensure the array will be well-defined
  maxBounds <- case nub $ map length heightRows of
    [singleCount] -> Right (length heightRows - 1, singleCount - 1)
    _ -> Left InputNotGridShape
  let rowsWithIndices = concatMap flattenRowIndex $ zip [0 ..] (map (zip [0 ..]) heightRows)
  pure $ array ((0, 0), maxBounds) rowsWithIndices
  where
    flattenRowIndex (row, colsWithIndices) =
      map (\(col, value) -> ((row, col), value)) colsWithIndices

readInput :: IO Grid
readInput = do
  content <- getContents
  case parseGrid content of
    Right result -> pure result
    Left err -> die (show err)

main :: IO ()
main = do
  grid <- readInput
  let low = findLowPoints grid
  print low
  putStrLn $ "Part1: " ++ show (computeGridRisk grid low)
  let basins = map (expandBasin grid) low
  print basins
  let basinSizes = map Set.size basins
  let productOfBiggest3 = product $ take 3 $ sortBy (flip compare) basinSizes
  putStrLn $ "Part2: " ++ show productOfBiggest3
