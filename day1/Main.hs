import Text.Read
import Data.Maybe

input :: IO [Int]
input = do
    allInput <- getContents
    return $ fromMaybe (error "Parse error") $ sequence $ map readMaybe $ lines allInput


deltas :: Num a => [a] -> [a]
deltas [] = []
deltas (x:xs) = go [] x xs where
    go acc current rest = case rest of
        [] -> acc
        x:rest' -> go (acc ++ [x - current]) x rest'

countIncs :: (Num a, Ord a) => [a] -> Int
countIncs = length . filter (> 0) . deltas

window :: Int -> [a] -> [[a]]
window size = go [] where
    go _ [] = []
    go currentWindow (x:xs) =
        let newWin = x : currentWindow in
            case compare (length newWin) size of
                LT -> go newWin xs
                EQ -> newWin : go newWin xs
                GT -> let trimmedWin = init newWin in trimmedWin : go trimmedWin xs

windowSum :: Num a => [a] -> [a]
windowSum = map sum . window 3

main :: IO ()
main = do
    depths <- input
    putStr "Increments: "
    print $ countIncs depths
    putStr "Windowed: "
    let windowed = windowSum depths
    print windowed
    print $ countIncs windowed

