import Data.Foldable (Foldable (toList), maximumBy)
import Data.Function (on)
import Data.Functor (($>))
import Data.Ix
import Data.Maybe (catMaybes, mapMaybe)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

type Vel = (Int, Int)

type Time = Int

type TimeRange = (Time, Time)

type PosRange = (Int, Int)

type Bounds = (PosRange, PosRange)

parseInput :: Parser Bounds
parseInput = do
  string "target area: x="
  xRange <- parseRange
  string ", y="
  yRange <- parseRange
  skipMany space
  eof
  pure (xRange, yRange)
  where
    parseInt = do
      sign <- (char '-' $> (-1)) <|> pure 1
      val <- read <$> many1 digit
      pure $ sign * val
    parseRange = do
      a <- parseInt
      string ".."
      b <- parseInt
      pure (a, b)

readInput :: IO Bounds
readInput = do
  input <- getContents
  case parse parseInput "" input of
    Left err -> die $ show err
    Right res -> pure res

solveQuad :: Int -> Int -> Maybe (Float, Float)
solveQuad position initVel =
  let b = fromIntegral $ 2 * initVel + 1
      discr = b * b - 8 * fromIntegral position
   in if discr < 0
        then Nothing
        else
          let d = sqrt discr
           in Just ((b - d) / 2, (b + d) / 2)

floatRangeToInt :: (RealFrac f, Integral n) => (f, f) -> Maybe (n, n)
floatRangeToInt (a, b) =
  let r@(a', b') = (ceiling a, floor b)
   in if a' <= b' then Just r else Nothing

timeInYRange :: PosRange -> Int -> [TimeRange]
timeInYRange (rangeMin, rangeMax) initVel =
  mapMaybe floatRangeToInt floatingTimes
  where
    solveSinglePos pos = solveQuad pos initVel

    floatingTimes = case (solveSinglePos rangeMin, solveSinglePos rangeMax) of
      (Just (enterMin, leaveMin), Just (leaveMax, enterMax)) ->
        [(enterMin, leaveMax), (enterMax, leaveMin)]
      (Just (enterMin, leaveMin), Nothing) -> [(enterMin, leaveMin)]
      (Nothing, Nothing) -> []
      (Nothing, Just _) -> error "Impossible"

timeInXRange :: PosRange -> Int -> Maybe TimeRange
timeInXRange (rangeMin, rangeMax) initVel =
  floatingTimes >>= floatRangeToInt >>= clampTimes
  where
    solveSinglePos pos = solveQuad pos initVel

    floatingTimes = case (solveSinglePos rangeMin, solveSinglePos rangeMax) of
      (Just (enterMin, _), Just (leaveMax, _)) -> Just (enterMin, leaveMax)
      (Just (enterMin, leaveMin), Nothing) -> Just (enterMin, leaveMin)
      (Nothing, Nothing) -> Nothing
      (Nothing, Just _) -> error "Impossible"

    clampTimes r@(start, end)
      | start > initVel = Nothing
      | end > initVel = Just (start, maxBound) -- TODO Open range instead of max
      | otherwise = Just r

intersect :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersect (a1, a2) (b1, b2) =
  let rangeMin = max a1 b1
      rangeMax = min a2 b2
   in if rangeMin <= rangeMax then Just (rangeMin, rangeMax) else Nothing

timeInTarget :: Bounds -> Vel -> [TimeRange]
timeInTarget (xRange, yRange) (xVel, yVel) =
  let xs = timeInXRange xRange xVel
      ys = timeInYRange yRange yVel
   in catMaybes [x `intersect` y | x <- toList xs, y <- ys]

willReachTarget :: Bounds -> Vel -> Bool
willReachTarget target initVel = not $ null $ timeInTarget target initVel

minXVel :: PosRange -> Int
minXVel (xMin, _) =
  -- Solve for the discriminant being positive
  ceiling $ (sqrt (8 * fromIntegral xMin) - 1) / 2

minYVelForXVel :: Bounds -> Int -> Int
minYVelForXVel (xRange, yRange) xVel =
  let Just (enterTime, _) = solveQuad (fst xRange) xVel
   in ceiling $
        fromIntegral (fst yRange) / enterTime + (enterTime - 1) / 2

maxYVel :: PosRange -> Int
maxYVel (yMin, _) = -yMin

maxXVel :: PosRange -> Int
maxXVel (_, xMax) = xMax

maxHeight :: Int -> Int
maxHeight yVel
  | yVel > 0 = yVel * (yVel + 1) `div` 2
  | otherwise = 0

getPossibleTrajectories :: Bounds -> [Vel]
getPossibleTrajectories target@(xRange, yRange) =
  [ (xVel, yVel)
    | xVel <- range (minXVel xRange, maxXVel xRange),
      yVel <- range (minYVelForXVel target xVel, maxYVel yRange)
  ]

getBestHeight :: [Vel] -> (Vel, Int)
getBestHeight =
  maximumBy (compare `on` snd) . map (\v@(_, yVel) -> (v, maxHeight yVel))

main :: IO ()
main = do
  target <- readInput
  let candidates = getPossibleTrajectories target
  let inTarget = filter (willReachTarget target) candidates
  let (bestVel, bestHeight) = getBestHeight inTarget
  putStrLn $ "Part1: " ++ show bestHeight
  putStrLn $ "Part2: " ++ show (length inTarget)
