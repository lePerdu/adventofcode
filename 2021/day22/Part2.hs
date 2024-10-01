import Data.Foldable
import Part1
  ( Coord,
    Cube (..),
    Input,
    OnOff (..),
    Range,
    Step (..),
    boolState,
    cubeCoords,
    cubeIntersect,
    cubeRange,
    rangeIntersect,
    readInput,
  )

isValidRange :: Range -> Bool
isValidRange (a, b) = a <= b

-- | Return parts of the first range which aren't in the second
rangeDiff :: Range -> Range -> [Range]
rangeDiff (oMin, oMax) (cutMin, cutMax) =
  let leftOut = (oMin, min oMax (cutMin - 1))
      rightOut = (max oMin (cutMax + 1), oMax)
   in if leftOut == rightOut
        then [leftOut]
        else filter isValidRange [leftOut, rightOut]

rangeDiffInt :: Range -> Range -> ([Range], [Range])
rangeDiffInt a b = (rangeDiff a b, toList $ rangeIntersect a b)

type Square = (Range, Range)

squareDiff :: Square -> Square -> [Square]
squareDiff (xRange, yRange) (cutXRange, cutYRange) =
  let xDiff = rangeDiff xRange cutXRange
      xInt = toList $ rangeIntersect xRange cutXRange
      yDiff = rangeDiff yRange cutYRange
      yInt = toList $ rangeIntersect yRange cutYRange
   in [(xs, yRange) | xs <- xDiff] ++ [(xs, ys) | xs <- xInt, ys <- yDiff]

-- | "Cut" one cube by another, returning the segments of the first cube which
-- are outside the second.
cubeCut :: Cube -> Cube -> [Cube]
cubeCut (Cube xRange yRange zRange) (Cube xCut yCut zCut) =
  let (xDiff, xInt) = rangeDiffInt xRange xCut
      (yDiff, yInt) = rangeDiffInt yRange yCut
      (zDiff, zInt) = rangeDiffInt zRange zCut
   in [Cube xs yRange zRange | xs <- xDiff]
        ++ [Cube xs ys zRange | xs <- xInt, ys <- yDiff]
        ++ [Cube xs ys zs | xs <- xInt, ys <- yInt, zs <- zDiff]

rangeLength :: Range -> Int
rangeLength (a, b) = b - a + 1

cubeVolume :: Cube -> Int
cubeVolume (Cube xRange yRange zRange) =
  rangeLength xRange * rangeLength yRange * rangeLength zRange

newtype Reactor = Reactor [Cube] deriving (Show)

countOn :: Reactor -> Int
countOn (Reactor cubes) = sum $ map cubeVolume cubes

applyStep :: Reactor -> Step -> Reactor
applyStep (Reactor onCells) (Step state stateCube) =
  let newOnCells = concat [cubeCut cell stateCube | cell <- onCells]
   in case state of
        On -> Reactor $ stateCube : newOnCells
        Off -> Reactor newOnCells

initReactor :: Reactor
initReactor = Reactor []

main :: IO ()
main = do
  steps <- readInput
  let finishedReactor = foldl' applyStep initReactor steps
  let onCount = countOn finishedReactor
  putStrLn $ "Part2: " ++ show onCount