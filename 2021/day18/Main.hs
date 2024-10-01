import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (Foldable (foldl'))
import Distribution.System (OS)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

data SnailNum = Pair SnailNum SnailNum | Number Int

instance Show SnailNum where
  show (Number n) = show n
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

leftmost :: SnailNum -> Int
leftmost (Number v) = v
leftmost (Pair l r) = leftmost l

rightmost :: SnailNum -> Int
rightmost (Number v) = v
rightmost (Pair l r) = rightmost r

applyLeft :: (Int -> Int) -> SnailNum -> SnailNum
applyLeft f (Number v) = Number (f v)
applyLeft f (Pair l r) = Pair (applyLeft f l) r

applyRight :: (Int -> Int) -> SnailNum -> SnailNum
applyRight f (Number v) = Number (f v)
applyRight f (Pair l r) = Pair l (applyRight f r)

data Context
  = CtxTop
  | CtxLeft Context SnailNum
  | CtxRight SnailNum Context

type Loc = (SnailNum, Context)

zipUp :: Loc -> SnailNum
zipUp (node, ctx) = case ctx of
  CtxTop -> node
  CtxLeft parent right -> zipUp (Pair node right, parent)
  CtxRight left parent -> zipUp (Pair left node, parent)

depth :: Context -> Int
depth CtxTop = 0
depth (CtxLeft parent _) = 1 + depth parent
depth (CtxRight _ parent) = 1 + depth parent

top :: SnailNum -> Loc
top t = (t, CtxTop)

mapLeft :: (Int -> Int) -> Context -> Context
mapLeft f CtxTop = CtxTop
mapLeft f (CtxLeft parent right) = CtxLeft (mapLeft f parent) right
mapLeft f (CtxRight left parent) = CtxRight (applyRight f left) parent

mapRight :: (Int -> Int) -> Context -> Context
mapRight f CtxTop = CtxTop
mapRight f (CtxLeft parent right) = CtxLeft parent (applyLeft f right)
mapRight f (CtxRight left parent) = CtxRight left (mapRight f parent)

data Reduced a = Reduced a | NotReduced deriving (Show, Eq, Functor)

pickFirstReduced :: Reduced a -> Reduced a -> Reduced a
pickFirstReduced a b = case (a, b) of
  (Reduced _, _) -> a
  (_, Reduced _) -> b
  _ -> NotReduced

reduceFirst :: (Loc -> Reduced Loc) -> Loc -> Reduced Loc
reduceFirst reduceNode orig@(node, context) =
  case reduceNode orig of
    r@(Reduced _) -> r
    NotReduced -> case node of
      Number _ -> NotReduced
      Pair a b ->
        pickFirstReduced
          (reduceFirst reduceNode (a, CtxLeft context b))
          (reduceFirst reduceNode (b, CtxRight a context))

explode :: Loc -> Reduced Loc
explode orig@(node, context) = case node of
  Pair (Number l) (Number r)
    | depth context >= 4 ->
        let context' = mapLeft (+ l) $ mapRight (+ r) context
         in Reduced (Number 0, context')
  _ -> NotReduced

split :: Loc -> Reduced Loc
split orig@(node, context) = case node of
  Number n
    | n >= 10 ->
        let half = fromIntegral n / 2
            left = Number $ floor half
            right = Number $ ceiling half
         in Reduced (Pair left right, context)
  _ -> NotReduced

reduceOnce :: SnailNum -> Reduced SnailNum
reduceOnce n =
  let loc = (n, CtxTop)
   in zipUp
        <$> pickFirstReduced
          (reduceFirst explode loc)
          (reduceFirst split loc)

reductionSteps :: SnailNum -> [SnailNum]
reductionSteps n =
  n : case reduceOnce n of
    Reduced n' -> reductionSteps n'
    NotReduced -> []

reduceFull :: SnailNum -> SnailNum
reduceFull = last . reductionSteps

snailMag :: SnailNum -> Int
snailMag (Number n) = n
snailMag (Pair l r) = 3 * snailMag l + 2 * snailMag r

snailAdd :: SnailNum -> SnailNum -> SnailNum
snailAdd a b = reduceFull $ Pair a b

snailSum :: [SnailNum] -> SnailNum
snailSum [] = Number 0
snailSum [x] = reduceFull x
snailSum (x : xs@(_ : _)) = foldl' snailAdd x xs

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x : rest) = concatMap (\y -> [(x, y), (y, x)]) rest ++ pairwise rest

parseSnailNum :: Parser SnailNum
parseSnailNum = parsePair <|> parseNum
  where
    parsePair = do
      char '['
      l <- parseSnailNum
      char ','
      r <- parseSnailNum
      char ']'
      pure $ Pair l r

    parseNum = Number . read <$> many1 digit

readInput :: IO [SnailNum]
readInput = do
  input <- getContents
  case parse (many1 (parseSnailNum <* optional newline)) "" input of
    Right res -> pure res
    Left err -> die $ show err

main :: IO ()
main = do
  input <- readInput
  let initSum = snailSum input
  print initSum
  let mag = snailMag initSum
  putStrLn $ "Part1: " ++ show mag
  let pairSums = map (snailMag . uncurry snailAdd) $ pairwise input
  let biggest = maximum pairSums
  putStrLn $ "Part2: " ++ show biggest
