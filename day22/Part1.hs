module Part1 where

import Control.Applicative (liftA2, liftA3)
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable (traverse_)
import Data.Functor
import Data.List (genericLength)
import Data.Maybe (mapMaybe)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

type Coord = (Int, Int, Int)

type Range = (Int, Int)

data Cube = Cube Range Range Range deriving (Show)

data OnOff = On | Off deriving (Show, Eq)

data Step = Step OnOff Cube deriving (Show)

type Reactor = UArray Coord Bool

type Input = [Step]

rangeIntersect :: Range -> Range -> Maybe Range
rangeIntersect (min1, max1) (min2, max2) =
  let min' = max min1 min2
      max' = min max1 max2
   in if min' <= max' then Just (min', max') else Nothing

cubeIntersect :: Cube -> Cube -> Maybe Cube
cubeIntersect (Cube xs1 ys1 zs1) (Cube xs2 ys2 zs2) =
  liftA3
    Cube
    (rangeIntersect xs1 xs2)
    (rangeIntersect ys1 ys2)
    (rangeIntersect zs1 zs2)

cubeCoords :: Cube -> [Coord]
cubeCoords (Cube xs ys zs) =
  [(x, y, z) | x <- range xs, y <- range ys, z <- range zs]

cubeRange :: Cube -> (Coord, Coord)
cubeRange (Cube (xMin, xMax) (yMin, yMax) (zMin, zMax)) =
  ((xMin, yMin, zMin), (xMax, yMax, zMax))

boolState :: OnOff -> Bool
boolState On = True
boolState Off = False

initArea :: Cube
initArea = let r = (-50, 50) in Cube r r r

initReactor :: Reactor
initReactor =
  let b = ((-50, -50, -50), (50, 50, 50))
   in array b [(c, False) | c <- range b]

applySteps :: [Step] -> Reactor -> Reactor
applySteps steps init = runSTUArray $ do
  reactor <- thaw init
  let reducedSteps = mapMaybe reduceStepArea steps
  traverse_ (applyStep reactor) reducedSteps
  pure reactor
  where
    reduceStepArea (Step state cube) =
      Step state <$> cubeIntersect initArea cube
    applyStep grid (Step state cube) =
      let s = boolState state
       in traverse_ (\c -> writeArray grid c s) (cubeCoords cube)

countOn :: Reactor -> Integer
countOn reactor = genericLength $ filter id $ elems reactor

parseInt :: Parser Int
parseInt = option id (char '-' $> negate) <*> (read <$> many1 digit)

parseRange :: Parser (Int, Int)
parseRange = do
  a <- parseInt
  string ".."
  b <- parseInt
  pure (a, b)

parseCube :: Parser Cube
parseCube =
  liftA3
    Cube
    (string "x=" *> parseRange)
    (string ",y=" *> parseRange)
    (string ",z=" *> parseRange)

parseStep :: Parser Step
parseStep = liftA2 Step parseOnOff parseCube
  where
    parseOnOff =
      char 'o' *> ((string "n" $> On) <|> (string "ff" $> Off)) <* skipMany1 space

parseInput :: Parser Input
parseInput = many (parseStep <* optional newline)

readInput :: IO Input
readInput = do
  contents <- getContents
  case parse parseInput "" contents of
    Left err -> die $ show err
    Right res -> pure res

main :: IO ()
main = do
  input <- readInput
  let finishedReactor = applySteps input initReactor
  let onCount = countOn finishedReactor
  putStrLn $ "Part1: " ++ show onCount