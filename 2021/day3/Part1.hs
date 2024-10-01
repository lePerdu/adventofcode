module Part1 (BitGrid, input, mostCommon, mostCommonInCol, allMostCommon, bitsToInt) where

import System.IO
import System.Exit

import Data.Foldable

type Error = String

type BitGrid = [[Bool]]

charToBit :: Char -> Either Error Bool
charToBit '0' = Right False
charToBit '1' = Right True
charToBit c = Left $ "Invalid bit: " ++ show c

lineToBits :: String -> Either Error [Bool]
lineToBits = traverse charToBit

mostCommon :: [Bool] -> Bool
mostCommon bits =
    let (total, trueCount) = foldl' combine (0, 0) bits
    in trueCount * 2 >= total
    where
        combine (total, trueCount) newBit =
            (total + 1, trueCount + fromEnum newBit)

mostCommonInCol :: Int -> BitGrid -> Bool
mostCommonInCol i grid = mostCommon (map (!! i) grid)

allMostCommon :: BitGrid -> [Bool]
allMostCommon grid@(first:rest) =
    let bitCount = length first
    in map (\c -> mostCommonInCol c grid) [0..bitCount-1]

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' (\acc b -> 2*acc + fromEnum b) 0

computeGammaEpsilon :: BitGrid -> (Int, Int)
computeGammaEpsilon grid =
    let gammaBits = allMostCommon grid
        epsilonBits = map not gammaBits
    in (bitsToInt gammaBits, bitsToInt epsilonBits)

input :: IO BitGrid
input = do
    contents <- getContents
    let inputRows = lines contents
    case traverse lineToBits inputRows of
        Right result -> pure result
        Left e -> hPutStrLn stderr e >> exitFailure

main :: IO ()
main = do
    grid <- input
    let (g, e) = computeGammaEpsilon grid
    putStrLn $ "Gamma: " ++ show g
    putStrLn $ "Epsilon: " ++ show e
    putStrLn $ "Part1: " ++ show (g*e)
