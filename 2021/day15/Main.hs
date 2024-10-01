import Data.Array.Unboxed
import Data.Char (ord)
import Data.Foldable (Foldable (foldl'))
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import System.Exit (die)

data InputError = InvalidChar Char | InputNotGridShape deriving (Show)

type Coord = (Int, Int)

type Path = [Coord]

data PathState = PathState
  { pathFrom :: Coord,
    pathRisk :: Int
  }
  deriving (Show)

type RiskCoord = (Int, Coord)

class Grid g where
  gridBounds :: g -> (Coord, Coord)
  unsafeGridRisk :: g -> Coord -> Int

  gridRisk :: g -> Coord -> Maybe Int
  gridRisk g ix =
    if inRange (gridBounds g) ix
      then Just (unsafeGridRisk g ix)
      else Nothing

instance Grid (UArray Coord Int) where
  gridBounds = bounds
  unsafeGridRisk = (!)

data DuplicatedGrid = DuplicatedGrid
  { dupGrid :: UArray Coord Int,
    dupTimes :: Int
  }

instance Grid DuplicatedGrid where
  gridBounds g =
    let ((0, 0), (maxRow, maxCol)) = bounds (dupGrid g)
        expandDim d = (d + 1) * dupTimes g - 1
     in ((0, 0), (expandDim maxRow, expandDim maxCol))
  unsafeGridRisk g (row, col) =
    let ((0, 0), (maxRow, maxCol)) = bounds (dupGrid g)
        (tileRow, innerRow) = row `divMod` (maxRow + 1)
        (tileCol, innerCol) = col `divMod` (maxCol + 1)
        incAmount = tileRow + tileCol
        inc r = (r + incAmount - 1) `mod` 9 + 1
     in inc $ unsafeGridRisk (dupGrid g) (innerRow, innerCol)

getAdjacent :: Coord -> [Coord]
getAdjacent (row, col) =
  [ (row + 1, col),
    (row - 1, col),
    (row, col + 1),
    (row, col - 1)
  ]

hamiltonian :: Coord -> Coord -> Int
hamiltonian a b = abs (fst a - fst b) + abs (snd a - snd b)

findPath :: Grid g => g -> Coord -> Coord -> Maybe (Path, Int)
findPath grid start end = astar initCoordStates initQueue
  where
    initCoordStates = Map.singleton start $ PathState start 0
    initQueue = Set.singleton (0, start)

    -- Hamiltonian distance is a lower bound for total risk
    heuristic :: Coord -> Int
    heuristic = hamiltonian end

    reconstructPath coordStates path@(headNode : _) risk =
      case Map.lookup headNode coordStates of
        Just (PathState nextNode _) ->
          if nextNode == headNode
            then (path, risk)
            else
              reconstructPath
                coordStates
                (nextNode : path)
                (unsafeGridRisk grid headNode + risk)
        Nothing -> (path, risk)

    astar coordStates queue = case Set.minView queue of
      Just ((_, current), rest) ->
        if current == end
          then Just $ reconstructPath coordStates [current] 0
          else
            let isBestPath (adj, state) = case Map.lookup adj coordStates of
                  Just existing -> pathRisk state < pathRisk existing
                  Nothing -> True
                currentRisk = maybe 0 pathRisk (Map.lookup current coordStates)
                makePathState adjacentCoord = do
                  coordRisk <- gridRisk grid adjacentCoord
                  let newRisk = coordRisk + currentRisk
                  let state = (adjacentCoord, PathState current newRisk)
                  if isBestPath state
                    then Just state
                    else Nothing
                candidates = mapMaybe makePathState (getAdjacent current)

                newCoordStates =
                  foldr (\(c, s) m -> Map.insert c s m) coordStates candidates

                insertIfNotPresent (coord, PathState _ risk) s =
                  if Set.member coord (Set.map snd s)
                    then s
                    else
                      let value = (risk + heuristic coord, coord)
                       in Set.insert value s
                newQueue = foldr insertIfNotPresent rest candidates
             in astar newCoordStates newQueue
      Nothing -> Nothing

parseRisk :: Char -> Either InputError Int
parseRisk c =
  if '0' <= c && c <= '9'
    then Right (ord c - ord '0')
    else Left (InvalidChar c)

parseGrid :: String -> Either InputError (UArray Coord Int)
parseGrid input = do
  heightRows <- traverse (traverse parseRisk) (lines input)
  -- Pre-compute bounds to ensure the array will be well-defined
  maxBounds <- case nub $ map length heightRows of
    [singleCount] -> Right (length heightRows - 1, singleCount - 1)
    _ -> Left InputNotGridShape
  let rowsWithIndices = concatMap flattenRowIndex $ zip [0 ..] (map (zip [0 ..]) heightRows)
  pure $ array ((0, 0), maxBounds) rowsWithIndices
  where
    flattenRowIndex (row, colsWithIndices) =
      map (\(col, value) -> ((row, col), value)) colsWithIndices

readInput :: IO (UArray Coord Int)
readInput = do
  content <- getContents
  case parseGrid content of
    Right result -> pure result
    Left err -> die (show err)

printPath :: Grid g => String -> g -> IO ()
printPath label grid =
  let (start, end) = gridBounds grid
      path = findPath grid start end
   in putStr label >> print (start, end) >> print path

main :: IO ()
main = do
  grid <- readInput
  printPath "Part1: " grid
  printPath "Part2: " $ DuplicatedGrid grid 5
