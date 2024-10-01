{-# LANGUAGE OverloadedStrings #-}

import Data.Bits (Bits (..))
import Data.Char
import Data.Foldable (Foldable (foldl'), find)
import Data.List (permutations)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Word
import Distribution.Simple.LocalBuildInfo qualified as Text
import System.Exit (die)

newtype DigitDisplay = DigitDisplay Word8 deriving (Show)

digitFromString :: String -> Maybe DigitDisplay
digitFromString chars = DigitDisplay <$> bitMask
  where
    charToBit c =
      if 'a' <= c && c <= 'g'
        then Just (1 `shift` (ord c - ord 'a'))
        else Nothing
    bits = traverse charToBit chars

    bitMask = fmap (foldl' (.|.) 0) bits

digitToString :: DigitDisplay -> String
digitToString (DigitDisplay bitMask) =
  let indices = filter (testBit bitMask) [0 .. 7]
   in map (\i -> chr (ord 'a' + i)) indices

digitToNumber :: DigitDisplay -> Maybe Int
digitToNumber (DigitDisplay bitMask) = case bitMask of
  0b1110111 -> Just 0
  0b0100100 -> Just 1
  0b1011101 -> Just 2
  0b1101101 -> Just 3
  0b0101110 -> Just 4
  0b1101011 -> Just 5
  0b1111011 -> Just 6
  0b0100101 -> Just 7
  0b1111111 -> Just 8
  0b1101111 -> Just 9
  _ -> Nothing

-- | "Easy" digits are 1, 4, 7, and 8
-- These are unique in the number of segments lit up (2, 4, 3, and 7
-- respectively).
isEasyDigit :: DigitDisplay -> Bool
isEasyDigit (DigitDisplay m) = popCount m `elem` [2, 3, 4, 7]

-- | Permutation of segments.
-- Segment at index `i` will be mapped to location permutation[`i`].
newtype Permutation = Permutation [Int] deriving (Show)

applyPerm :: Permutation -> DigitDisplay -> DigitDisplay
applyPerm (Permutation srcToDst) (DigitDisplay bitMask) =
  DigitDisplay $ foldl' (.|.) 0 $ map newBitMask [0 .. 7]
  where
    newBitMask bitIndex =
      if testBit bitMask bitIndex
        then bit (srcToDst !! bitIndex)
        else 0

isPermValidFor :: Permutation -> [DigitDisplay] -> Bool
isPermValidFor perm = all (isJust . digitToNumber . applyPerm perm)

allPermutations :: [Permutation]
allPermutations = map Permutation $ permutations [0 .. 7]

findValidPermutation :: [DigitDisplay] -> Maybe Permutation
findValidPermutation allDigitDisplays =
  find (`isPermValidFor` allDigitDisplays) allPermutations

data InputLine = InputLine
  { inputPatterns :: [DigitDisplay],
    outputPatterns :: [DigitDisplay]
  }
  deriving (Show)

type Input = [InputLine]

findCorrectOutputDigits :: InputLine -> Maybe [Int]
findCorrectOutputDigits inputLine = do
  foundPerm <- findValidPermutation (inputPatterns inputLine)
  let permutedOutputs = map (applyPerm foundPerm) (outputPatterns inputLine)
  traverse digitToNumber permutedOutputs

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\acc digit -> acc * 10 + digit) 0

parseLine :: Text.Text -> Either String InputLine
parseLine l = case Text.split (== '|') l of
  [inputs, outputs] ->
    let inputWords = Text.words inputs
        outputWords = Text.words outputs
     in if length inputWords == 10 && length outputWords == 4
          then
            InputLine
              <$> convertWordsToDigits inputWords
              <*> convertWordsToDigits outputWords
          else Left "Wrong number of words. Expected 10 inputs and 4 outputs"
  _ -> Left "Expected `input patterns | output patterns`"
  where
    convertWordsToDigits =
      traverse
        ( maybe (Left "Invalid digit display") Right
            . digitFromString
            . Text.unpack
        )

readInput :: IO Input
readInput = do
  contents <- TextIO.getContents
  let nonEmptyLines = filter (not . Text.null . Text.strip) (Text.lines contents)
  case traverse parseLine nonEmptyLines of
    Right result -> pure result
    Left err -> die err

main :: IO ()
main = do
  input <- readInput
  let easyOutputs = filter isEasyDigit $ concatMap outputPatterns input
  putStrLn $ "Part1: " ++ show (length easyOutputs)

  case traverse findCorrectOutputDigits input of
    Just outputDigitList -> do
      let outputNumbers = map digitsToInt outputDigitList
      let answer = sum outputNumbers
      putStrLn $ "Part2: " ++ show answer
    Nothing -> die "Could not find permutations"
