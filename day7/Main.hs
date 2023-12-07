import Data.Foldable (minimumBy)
import Data.Function (on)
import Text.XHtml (input)

type Position = Int

type CostFunc = Position -> Position -> Fuel

newtype Fuel = Fuel {getFuel :: Int}
  deriving (Eq, Ord, Num, Enum, Real, Integral)

totalFuelForDest :: CostFunc -> Position -> [Position] -> Fuel
totalFuelForDest cost dest = sum . map (`cost` dest)

findOptimalDest :: CostFunc -> [Position] -> (Position, Fuel)
findOptimalDest cost positions = minimumBy (compare `on` snd) candidates
  where
    potentialPositions = [minimum positions .. maximum positions]
    candidates =
      map (\d -> (d, totalFuelForDest cost d positions)) potentialPositions

linearCost :: CostFunc
linearCost src dst = Fuel $ abs (src - dst)

quadCost :: CostFunc
quadCost src dst =
  let distance = linearCost src dst
   in distance * (distance + 1) `div` 2

readInput :: IO [Position]
readInput = fmap (\contents -> read ("[" ++ contents ++ "]")) getContents

outputAns :: String -> CostFunc -> [Position] -> IO ()
outputAns label cost input = do
  let (pos, fuel) = findOptimalDest cost input
  putStrLn $ label ++ ": " ++ show (getFuel fuel)

main :: IO ()
main = do
  input <- readInput
  outputAns "Part1" linearCost input
  outputAns "Part2" quadCost input
