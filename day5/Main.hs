{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array.Unboxed
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit
import Text.Parsec
import Text.Parsec.Text (Parser)

data Coord = Coord {xCoord :: Int, yCoord :: Int} deriving (Show, Eq)

type LineSegment = (Coord, Coord)

newtype Input = Input [LineSegment]

type Grid = UArray (Int, Int) Int

isAxisAligned :: LineSegment -> Bool
isAxisAligned (Coord sx sy, Coord ex ey) = sx == ex || sy == ey

getAllPoints :: LineSegment -> [Coord]
getAllPoints (start, end) = doStep start
  where
    dx = xCoord end - xCoord start
    dy = yCoord end - yCoord start
    stepX = signum dx
    stepY = signum dy

    doStep coord =
      if coord == end
        then [coord]
        else
          let nextCoord = Coord (xCoord coord + stepX) (yCoord coord + stepY)
           in coord : doStep nextCoord

makeGrid :: [LineSegment] -> Grid
makeGrid ventLines =
  accumArray
    (+) -- Accumulating function
    0 -- Default/init value
    ((0, 0), (rowCount, colCount)) -- Bounds
    -- Coords paired with increments (always 1 here)
    (map (\c -> ((yCoord c, xCoord c), 1)) allCoords)
  where
    allCoords = concatMap getAllPoints ventLines
    rowCount = maximum $ map yCoord allCoords
    colCount = maximum $ map xCoord allCoords

drawGrid :: Grid -> T.Text
drawGrid grid = fullString
  where
    charAt ventCount = case ventCount of
      0 -> '.'
      _ | 0 < ventCount && ventCount < 10 -> head (show ventCount)
      _ -> '#'

    ((minRow, minCol), (maxRow, maxCol)) = bounds grid
    rowString row =
      T.pack $ map (\col -> charAt (grid ! (row, col))) [minCol .. maxCol]
    fullString = T.intercalate "\n" $ map rowString [minRow .. maxRow]

parseInput :: Parser Input
parseInput = Input <$> many parseLine <* eof
  where
    parseInt = read <$> many1 digit

    parseCoord = do
      x <- parseInt
      char ','
      y <- parseInt
      pure $ Coord {xCoord = x, yCoord = y}

    parseLine = do
      start <- parseCoord
      string " -> "
      end <- parseCoord
      optional endOfLine
      pure (start, end)

readInput :: IO Input
readInput = do
  contents <- TIO.getContents
  case parse parseInput "-" contents of
    Right result -> pure result
    Left err -> die (show err)

countOver2 :: Grid -> Int
countOver2 grid = length $ filter (>= 2) $ elems grid

main :: IO ()
main = do
  Input lines <- readInput
  let axisLines = filter isAxisAligned lines
  let grid = makeGrid axisLines
  -- TIO.putStrLn $ drawGrid grid
  let deadly = countOver2 grid
  putStrLn $ "Part1: " ++ show deadly

  let fullGrid = makeGrid lines
  let deadly = countOver2 fullGrid
  putStrLn $ "Part2: " ++ show deadly
