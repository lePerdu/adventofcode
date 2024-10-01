module Part1 where

newtype FishCycle = FishCycle Int deriving (Eq)

-- | Naive, direct representation of the school of fish
newtype NaiveSchool = NaiveSchool {getNaiveFish :: [FishCycle]}

-- | More optimized representation of the school
-- Counts how many fish are at each stage of their life-cycle
newtype School = School {getFishCounts :: [Integer]}

newbornCounter :: Int = 8

resetCounter :: Int = 6

advanceFish :: FishCycle -> (FishCycle, Bool)
advanceFish (FishCycle 0) = (FishCycle resetCounter, True)
advanceFish (FishCycle n) = (FishCycle (n - 1), False)

class FishSim s where
  makeSim :: [FishCycle] -> s
  advanceSim :: s -> s
  countFish :: s -> Int

instance FishSim NaiveSchool where
  makeSim = NaiveSchool
  advanceSim = NaiveSchool . concatMap advanceWithNewborn . getNaiveFish
    where
      advanceWithNewborn f =
        let (newCycle, hasNewborn) = advanceFish f
         in if hasNewborn
              then [newCycle, FishCycle newbornCounter]
              else [newCycle]
  countFish = length . getNaiveFish

instance FishSim School where
  makeSim allFish = School $ map countWithCycle [0 .. newbornCounter]
    where
      countWithCycle cycle =
        toInteger $ length $ filter (== FishCycle cycle) allFish

  advanceSim (School (cycleZero : rest)) =
    School $ withNewborns $ withResets rest
    where
      -- Shift fish with cycle of 0 back to where they go, and add in newborns
      withNewborns fish = fish ++ [cycleZero]
      incrementMatching index value =
        if index == resetCounter then value + cycleZero else value
      withResets = zipWith incrementMatching [0 ..]

  countFish = fromInteger . sum . getFishCounts

readInput :: IO [Int]
readInput = do
  content <- getContents
  pure $ read $ "[" ++ content ++ "]"

main :: IO ()
main = do
  input <- readInput
  let sim = makeSim (map FishCycle input) :: School
  let simStates = iterate advanceSim sim
  let part1Count = countFish (simStates !! 80)
  putStrLn $ "Part1: " ++ show part1Count
  let part2Count = countFish (simStates !! 256)
  putStrLn $ "Part2: " ++ show part2Count