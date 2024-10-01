module Part2 where

import Data.Bits
import Data.Foldable
import Data.Function
import Data.List

import Part1

indexMatches :: Eq a => Int -> [a] -> [a] -> Bool
indexMatches i = (==) `on` (!! i)

applyBitCriteria :: Bool -> BitGrid -> Either String [Bool]
applyBitCriteria invertMatch grid = case foldl' foldFn grid allIndices of
    [single] -> Right single
    [] -> Left "None matched"
    _ -> Left "More than 1 matched" 
    where
        allIndices = [0..length (transpose grid)]

        foldFn remaining@[_single] _ = remaining
        foldFn remaining bitIndex =
            let bitToMatch = mostCommonInCol bitIndex remaining
                matchesBit bits =
                    bits !! bitIndex == bitToMatch `xor` invertMatch
            in filter matchesBit remaining

computeLifeSupportRating :: BitGrid -> Either String Int
computeLifeSupportRating grid = do
    oxygenRating <- applyBitCriteria False grid
    co2Rating <- applyBitCriteria True grid
    pure $ bitsToInt oxygenRating * bitsToInt co2Rating

main :: IO ()
main = do
    grid <- input
    let oxygenRating = applyBitCriteria False grid
    let co2Rating = applyBitCriteria True grid
    putStrLn $ "O2: " ++ show oxygenRating
    putStrLn $ "CO2: " ++ show co2Rating
    case (oxygenRating, co2Rating) of
        (Right o, Right c)->
            let ans = bitsToInt o * bitsToInt c
            in putStrLn $ "Part2: " ++ show ans
        _ -> pure ()
   