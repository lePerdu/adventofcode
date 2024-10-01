import Control.Applicative (Applicative (liftA2), liftA3)
import Data.Binary.Get (remaining)
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.Functor (($>))
import Data.List (foldl1', nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

data V3 a = V3 !a !a !a deriving (Eq, Ord, Functor)

instance Show a => Show (V3 a) where
  show (V3 a b c) = "<" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ">"

instance Applicative V3 where
  pure v = V3 v v v
  V3 fx fy fz <*> V3 x y z = V3 (fx x) (fy y) (fz z)

instance Foldable V3 where
  foldMap f (V3 x y z) = f x <> f y <> f z

newtype Rot a = Rot (V3 (V3 a)) deriving (Eq, Functor, Show)

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance Num a => Semigroup (V3 a) where
  (<>) = (+)

instance Num a => Monoid (V3 a) where
  mempty = pure 0

dot :: Num a => V3 a -> V3 a -> a
a `dot` b = sum (a + b)

transposeM :: Rot a -> Rot a
transposeM (Rot (V3 (V3 a11 a12 a13) (V3 a21 a22 a23) (V3 a31 a32 a33))) =
  Rot $ V3 (V3 a11 a21 a31) (V3 a12 a22 a32) (V3 a13 a23 a33)

inverse :: Rot a -> Rot a
inverse = transposeM -- For rotations

rotateV :: Num a => Rot a -> V3 a -> V3 a
rotateV (Rot rows) v = fmap (\r -> sum (r * v)) rows

instance Num a => Semigroup (Rot a) where
  Rot aRows <> b =
    let b' = transposeM b
     in Rot $ fmap (rotateV b') aRows

instance Num a => Monoid (Rot a) where
  mempty = Rot $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

norm2 :: Num a => V3 a -> a
norm2 = sum . fmap (\x -> x * x)

type Coord = V3 Int

type Orient = Rot Int

rotX :: Orient
rotX = Rot $ V3 (V3 1 0 0) (V3 0 0 (-1)) (V3 0 1 0)

rotY :: Orient
rotY = Rot $ V3 (V3 0 0 1) (V3 0 1 0) (V3 (-1) 0 0)

rotZ :: Orient
rotZ = Rot $ V3 (V3 0 (-1) 0) (V3 1 0 0) (V3 0 0 1)

allRotations :: [Orient]
allRotations = nub allRots
  where
    times4 r = let r2 = r <> r in [mempty, r, r2, r2 <> r]
    -- Compute all rotations "euler angle" style, then remove duplicates later
    allRots =
      [ x <> y <> z
        | x <- times4 rotX,
          y <- times4 rotY,
          z <- times4 rotZ
      ]

newtype Scan = Scan {scanBeacons :: [Coord]}
  deriving (Show)

rotateScan :: Orient -> Scan -> Scan
rotateScan rot (Scan beacons) = Scan $ map (rotateV rot) beacons

allScanOrients :: Scan -> [Scan]
allScanOrients s = map (`rotateScan` s) allRotations

pairs :: [a] -> [b] -> [(a, b)]
pairs [] _ = []
pairs (a : as) bs = [(a, b) | b <- bs] ++ pairs as bs

data Overlap = Overlap {overlapOffset :: Coord, overlapBeacons :: [Coord]}
  deriving (Show)

overlapFromMap :: Map Coord [Coord] -> [Overlap]
overlapFromMap = map (uncurry Overlap) . Map.toList

getBoundingBox :: [Coord] -> (Coord, Coord)
getBoundingBox = foldl' accMinMax (pure maxBound, pure minBound)
  where
    accMinMax (accMin, accMax) new =
      (liftA2 min accMin new, liftA2 max accMax new)

-- | All possible offsets from s1 -> s2 such that at least 1 beacon is seen by
-- both
getPotentialOffsets :: Scan -> Scan -> [Overlap]
getPotentialOffsets (Scan s1) (Scan s2) =
  overlapFromMap $ foldl' mergeBeacon Map.empty $ pairs s1 s2
  where
    addOrNew b existing = b : fromMaybe [] existing
    mergeBeacon mapping (b1, b2) =
      let off = b1 - b2
       in Map.alter (Just . addOrNew b1) off mapping

distance :: Coord -> Coord -> Int
distance a b = sum $ abs $ a - b

scannerRange :: Int = 1000

isOverlapSignificant :: Overlap -> Bool
isOverlapSignificant (Overlap _ beacons) = length beacons >= 12

isOverlapConsistent :: Scan -> Scan -> Overlap -> Bool
isOverlapConsistent (Scan s1) (Scan s2) (Overlap offset commonBeacons) =
  all (inRangeIncludedInCommon mempty) s1
    && all (inRangeIncludedInCommon offset) s2
  where
    inRangeOf scanner b = distance b scanner < scannerRange
    inRangeIncludedInCommon scannerLoc b =
      not (inRangeOf scannerLoc b) || b `elem` commonBeacons

findPossibleOverlaps :: Scan -> Scan -> [(Scan, Overlap)]
findPossibleOverlaps s1 s2 =
  concatMap (\s2' -> map (s2',) $ findOverlapsForOriented s2') $
    allScanOrients s2
  where
    findOverlapsForOriented s2' =
      -- filter (isOverlapConsistent s1 s2) $
      filter isOverlapSignificant $
        -- Assume s1 is at a fixed / "proper" orientation
        getPotentialOffsets s1 s2'

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f = go ([], [])
  where
    go acc [] = acc
    go (bs, as) (x : rest) = case f x of
      Just b -> go (b : bs, as) rest
      Nothing -> go (bs, x : as) rest

orientAndPositionScans :: Scan -> [Scan] -> [(Scan, Coord)]
orientAndPositionScans originScan = go [(originScan, mempty)]
  where
    findOverlapsFromBase :: Scan -> (Scan, Coord) -> [(Scan, Coord)]
    findOverlapsFromBase newScan (baseScan, baseOffset) =
      map (\(oriented, Overlap offset _) -> (oriented, baseOffset + offset)) $
        findPossibleOverlaps baseScan newScan
    tryOrientScan existing new =
      case concatMap (findOverlapsFromBase new) existing of
        [] -> Nothing
        (firstMatch : _) -> Just firstMatch

    go :: [(Scan, Coord)] -> [Scan] -> [(Scan, Coord)]
    go existing [] = existing
    go existing remaining =
      let (newlyConnected, remaining') = partitionMaybe (tryOrientScan existing) remaining
       in if null newlyConnected
            then error "Could not extend map"
            else go (existing ++ newlyConnected) remaining'

dedupBeacons :: [(Scan, Coord)] -> [Coord]
dedupBeacons offsetScans =
  nub $ concatMap (\(Scan bs, offset) -> map (+ offset) bs) offsetScans

findMaxDistance :: [Coord] -> Int
findMaxDistance offsets =
  let scanPairs = pairs offsets offsets
      distances = map (uncurry distance) scanPairs
   in maximum distances

type ScannerId = Int

type Input = Map ScannerId [Coord]

parseInt :: Parser Int
parseInt =
  option id (char '-' $> negate) <*> (read <$> many1 digit)

parseCoord :: Parser Coord
parseCoord = liftA3 V3 (parseInt <* char ',') (parseInt <* char ',') parseInt

parseScanner :: Parser (ScannerId, [Coord])
parseScanner = do
  string "--- scanner "
  sId <- parseInt
  string " ---"
  newline
  coords <- many (parseCoord <* optional newline)
  pure (sId, coords)

parseInput :: Parser Input
parseInput = Map.fromList <$> many (parseScanner <* skipMany newline)

readInput :: IO Input
readInput = do
  contents <- getContents
  case parse parseInput "" contents of
    Left err -> die $ show err
    Right res -> pure res

main :: IO ()
main = do
  scans <- readInput
  let s0 = Scan $ fromJust $ Map.lookup 0 scans
  let restScans = Map.delete 0 scans
  let resolvedScans = orientAndPositionScans s0 (map Scan $ Map.elems restScans)
  let allBeacons = dedupBeacons resolvedScans
  let beaconCount = length allBeacons
  putStrLn $ "Part1: " ++ show beaconCount
  putStrLn $ "Part2: " ++ show (findMaxDistance $ map snd resolvedScans)
