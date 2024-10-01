import Data.Array.Unboxed
import Data.Array.Unboxed (array)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Foldable (Foldable (foldl'))
import Data.Function (on)
import Data.List (intercalate)
import Data.Text.IO qualified as TextIO
import System.Exit (die)
import Text.Parsec
import Text.Parsec.Text (Parser)

type Coord = (Int, Int)

type Bounds = (Coord, Coord)

data Axis = AxisX | AxisY deriving (Show, Eq)

data Fold = Fold {foldAxis :: Axis, foldPosition :: Int} deriving (Show)

data Input = Input
  { inputCoords :: [Coord],
    inputFolds :: [Fold]
  }
  deriving (Show)

type Grid = UArray Coord Bool

buildGrid :: [Coord] -> Grid
buildGrid coords =
  accumArray
    (||)
    False
    ((xMin, yMin), (xMax, yMax))
    (map (,True) coords)
  where
    xMin = minimum $ map fst coords
    xMax = maximum $ map fst coords
    yMin = minimum $ map snd coords
    yMax = maximum $ map snd coords

splitAtFold :: Bounds -> Fold -> (Bounds, Bounds)
splitAtFold ((minX, minY), (maxX, maxY)) (Fold axis pos) =
  case axis of
    AxisX ->
      (((minX, minY), (pos - 1, maxY)), ((pos + 1, minY), (maxX, maxY)))
    AxisY ->
      (((minX, minY), (maxX, pos - 1)), ((minX, pos + 1), (maxX, maxY)))

flipAroundFold :: Bounds -> Fold -> (Bounds, Coord -> Coord)
flipAroundFold ((minX, minY), (maxX, maxY)) (Fold axis pos) =
  case axis of
    AxisX ->
      ( ((flipSingleCoord maxX, minY), (flipSingleCoord minX, maxY)),
        first flipSingleCoord
      )
    AxisY ->
      ( ((minX, flipSingleCoord maxY), (maxX, flipSingleCoord minY)),
        second flipSingleCoord
      )
  where
    flipSingleCoord i = 2 * pos - i

applyFold :: Grid -> Fold -> Grid
applyFold grid fold =
  let gridBounds = bounds grid
      (sub1, sub2) = splitAtFold gridBounds fold
      (sub2Flipped, mapSub2Ix) = flipAroundFold sub2 fold
      applyOn f select = (f `on` select) sub1 sub2Flipped
      fullBounds = (min `applyOn` fst, max `applyOn` snd)
      sub1Assocs = [(i, grid ! i) | i <- range sub1]
      sub2Assocs = [(mapSub2Ix i, grid ! i) | i <- range sub2]
   in accumArray (||) False fullBounds $ sub1Assocs ++ sub2Assocs

applyFolds :: Grid -> [Fold] -> Grid
applyFolds = foldl' applyFold

showGrid :: Grid -> String
showGrid grid =
  let ((minX, minY), (maxX, maxY)) = bounds grid
      showCell y x = if grid ! (x, y) then '#' else '.'
      showRow y = map (showCell y) [minX .. maxX]
   in intercalate "\n" $ map showRow [minY .. maxY]

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseCoord :: Parser Coord
parseCoord = do
  x <- parseInt
  char ','
  y <- parseInt
  pure (x, y)

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  axis <- AxisX <$ char 'x' <|> AxisY <$ char 'y'
  char '='
  pos <- parseInt
  pure $ Fold {foldAxis = axis, foldPosition = pos}

parseInput :: Parser Input
parseInput = do
  coords <- many (parseCoord <* spaces)
  folds <- many (parseFold <* spaces)
  eof
  pure $ Input {inputCoords = coords, inputFolds = folds}

readInput :: IO Input
readInput = do
  result <- parse parseInput "stdin" <$> TextIO.getContents
  case result of
    Right input -> pure input
    Left err -> die $ show err

main :: IO ()
main = do
  input <- readInput
  -- print input
  let grid = buildGrid $ inputCoords input
  let folded = applyFold grid (head (inputFolds input))
  -- putStrLn "Initial:"
  -- putStrLn $ showGrid grid
  -- putStrLn "Folded:"
  -- putStrLn $ showGrid folded
  let dotCount = length $ filter id $ elems folded
  putStrLn $ "Part1: " ++ show dotCount

  let fullyFolded = applyFolds grid (inputFolds input)
  putStrLn $ "Part2:"
  putStrLn $ showGrid fullyFolded
