import Control.Applicative (Applicative (liftA2))
import Data.Array.Unboxed
import Data.Functor (($>))
import Data.List (foldl', intercalate)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

newtype Algorithm = Algorithm (UArray Int Bool) deriving (Show)

data Image = Image
  { imageFiniteGrid :: UArray (Int, Int) Bool,
    imageDefaultPixel :: Bool
  }

data Input = Input {inputAlg :: Algorithm, inputImage :: Image} deriving (Show)

instance Show Image where
  show image@(Image grid def) =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds grid
        showPixel c = let p = getPixel image c in if p then '#' else '.'
     in intercalate
          "\n"
          [ [showPixel (row, col) | col <- [minCol - 1 .. maxCol + 1]]
            | row <- [minRow - 1 .. maxRow + 1]
          ]

getAdjacent :: (Int, Int) -> [(Int, Int)]
getAdjacent (row, col) = range ((row - 1, col - 1), (row + 1, col + 1))

getPixel :: Image -> (Int, Int) -> Bool
getPixel (Image grid def) coord =
  if inRange (bounds grid) coord
    then grid ! coord
    else def

pixelId :: Image -> (Int, Int) -> Int
pixelId image coord =
  let values = [getPixel image c | c <- getAdjacent coord]
   in foldl' (\acc b -> 2 * acc + if b then 1 else 0) 0 values

maxAlgInput :: Int = 511

parsePixel :: Parser Bool
parsePixel = (char '.' $> False) <|> (char '#' $> True)

enhanceImage :: Algorithm -> Image -> Image
enhanceImage (Algorithm algTable) image@(Image grid def) =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds grid
      newBounds = ((minRow - 1, minCol - 1), (maxRow + 1, maxCol + 1))
      getNewPixel coord = algTable ! pixelId image coord
      newDefault = algTable ! (if def then maxAlgInput else 0)
   in Image
        { imageFiniteGrid =
            array
              newBounds
              [(coord, getNewPixel coord) | coord <- range newBounds],
          imageDefaultPixel = newDefault
        }

countPixels :: Image -> Maybe Int
countPixels (Image grid def)
  | def = Nothing
  | not def = Just $ length $ filter id $ elems grid

parseAlg :: Parser Algorithm
parseAlg = toAlg <$> count (maxAlgInput + 1) pixelWithSpaces
  where
    toAlg = Algorithm . listArray (0, maxAlgInput)

    -- Allow optional line breaks
    pixelWithSpaces = parsePixel <* optional newline

parseImage :: Parser Image
parseImage = do
  allRows <- many1 parseRow
  let rowCount = length allRows
  let colCount = length (head allRows)
  -- TODO Ensure all rows are the same length
  let allPixels = concat allRows
  pure $
    Image
      { imageFiniteGrid = listArray ((1, 1), (rowCount, colCount)) allPixels,
        imageDefaultPixel = False
      }
  where
    parseRow = many1 parsePixel <* newline

parseInput :: Parser Input
parseInput = do
  alg <- parseAlg
  skipMany1 newline
  img <- parseImage
  eof
  pure $ Input alg img

readInput :: IO Input
readInput = do
  contents <- getContents
  case parse parseInput "" contents of
    Left err -> die $ show err
    Right res -> pure res

printResult :: String -> Image -> IO ()
printResult label img = do
  print img
  case countPixels img of
    Just p -> putStrLn $ label ++ ": " ++ show p
    Nothing -> putStrLn $ "No result for " ++ label

main :: IO ()
main = do
  Input alg img <- readInput
  print img
  let enhancements = iterate (enhanceImage alg) img
  printResult "Part1" (enhancements !! 2)
  printResult "Part2" (enhancements !! 50)
