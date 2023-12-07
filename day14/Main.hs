import Control.Monad (forM_)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (Foldable (foldl'), maximumBy, minimumBy)
import Data.Function (on)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq ((:<|)), ViewL (..), ViewR (..), (<|), (><), (|>))
import Data.Sequence qualified as Seq
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String (Parser)

type Element = Char

type Polymer = Seq.Seq Element

type Rule = ((Element, Element), Element)

type Rules = Map.Map (Element, Element) Element

data ElementCounts = ElementCounts
  { ecPairs :: Map.Map (Element, Element) Integer,
    -- | Keep track of first and last since they aren't counted twice in pairs
    ecFirst, ecLast :: Element
  }

instance Show ElementCounts where
  show = show . ecPairs

data Input = Input
  { inputTemplate :: Polymer,
    inputRules :: Rules
  }
  deriving (Show)

incAt :: (Ord k, Integral n) => k -> Map.Map k n -> Map.Map k n
incAt = incBy 1

incBy :: (Ord k, Integral n) => n -> k -> Map.Map k n -> Map.Map k n
incBy count = Map.alter (\existing -> Just (fromMaybe 0 existing + count))

applyRules :: Rules -> Polymer -> Polymer
applyRules rules (a :<| b :<| rest) = fold Seq.empty a b rest
  where
    maybeInserted a b = maybe Seq.empty Seq.singleton $ Map.lookup (a, b) rules
    fold result a b (c :<| rest) =
      fold (result >< a <| maybeInserted a b) b c rest
    fold result a b Seq.Empty = result >< a <| (maybeInserted a b |> b)
applyRules _ singlePolymer = singlePolymer

applyRulesToCounts :: Rules -> ElementCounts -> ElementCounts
applyRulesToCounts rules elCounts =
  let expandPair p@(a, b) = case Map.lookup p rules of
        Just new -> [(a, new), (new, b)]
        Nothing -> [p]
      expandAndFlatten (pair, count) = [(p, count) | p <- expandPair pair]
      expandedPairs = concatMap expandAndFlatten $ Map.toList $ ecPairs elCounts
      combine newPairCounts (newPair, count) = incBy count newPair newPairCounts
      accumulated = foldl' combine Map.empty expandedPairs
   in elCounts {ecPairs = accumulated}

toElementCounts :: Polymer -> ElementCounts
toElementCounts polymer@(a :<| b :<| rest) =
  ElementCounts {ecPairs = pairCounts, ecFirst = first, ecLast = last}
  where
    first = a
    _ :> last = Seq.viewr polymer -- Won't fail since the polymer is non-empty
    fold result a b (c :<| rest) = fold (incAt (a, b) result) b c rest
    fold result a b Seq.Empty = incAt (a, b) result
    pairCounts = fold Map.empty a b rest

getPerElementCounts :: ElementCounts -> Map.Map Element Integer
getPerElementCounts elCounts =
  let flat ((a, b), count) = [(a, count), (b, count)]
      flattenedPairCounts = concatMap flat $ Map.toList $ ecPairs elCounts
      -- This counts everything but the endpoints twice, so we need to fix that
      rawCounts = foldl' (\m (p, c) -> incBy c p m) Map.empty flattenedPairCounts
      withExtraFirst = incAt (ecFirst elCounts)
      withExtraLast = incAt (ecLast elCounts)
      halved = fmap (`div` 2)
      transform = halved . withExtraFirst . withExtraLast
   in transform rawCounts

commonalityDiff :: ElementCounts -> Integer
commonalityDiff counts =
  let assocs = Map.assocs $ getPerElementCounts counts
      getFreq = compare `on` snd
      (_, most) = maximumBy getFreq assocs
      (_, least) = minimumBy getFreq assocs
   in most - least

parseElement :: Parser Element
parseElement = upper

parseRule :: Parser Rule
parseRule = do
  a <- parseElement
  b <- parseElement
  string " -> "
  res <- parseElement
  pure ((a, b), res)

parseInput :: Parser Input
parseInput = do
  templateChars <- many1 parseElement
  many1 newline
  rules <- many (parseRule <* newline)
  eof
  pure $ Input (Seq.fromList templateChars) (Map.fromList rules)

readInput :: IO Input
readInput = do
  result <- parse parseInput "stdin" <$> getContents
  case result of
    Right input -> pure input
    Left err -> die $ show err

main :: IO ()
main = do
  input <- readInput
  let elCounts = toElementCounts (inputTemplate input)
  let steps = iterate (applyRulesToCounts (inputRules input)) elCounts
  -- forM_ (take 5 steps) print
  let part1 = steps !! 10
  putStrLn $ "Part1: " ++ show (commonalityDiff part1)
  let part2 = steps !! 40
  putStrLn $ "Part2: " ++ show (commonalityDiff part2)
