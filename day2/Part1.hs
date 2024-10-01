module Part1 (Command(..), Coord(..), input, applyCommand) where

import Text.Read ( readEither )

data Command = Forward Int | Up Int | Down Int deriving (Show)

data Coord = Coord { depth :: Int, pos :: Int } deriving (Show)

applyCommand :: Command -> Coord -> Coord
applyCommand command (Coord { depth, pos }) = case command of
    Forward x -> Coord { depth, pos = pos + x }
    Up y -> Coord { depth = depth - y, pos }
    Down y -> Coord {depth = depth + y, pos}


parseCommand :: String -> Either String Command
parseCommand s = case words s of
    ["forward", arg] -> Forward <$> readEither arg
    ["up", arg] -> Up <$> readEither arg
    ["down", arg] -> Down <$> readEither arg
    _ -> Left $ "Invalid command: " ++ s


input :: IO [Command]
input = do
    contents <- getContents
    let commandStrings = lines contents
    let commands = traverse parseCommand commandStrings
    case commands of
        Left e -> error e
        Right c -> pure c


main :: IO ()
main = do
    commands <- input
    let finalCoord = foldl (flip applyCommand) (Coord { depth = 0, pos = 0 }) commands
    putStrLn $ "Final coord: " ++ show finalCoord
    putStrLn $ "Part1: " ++ show (depth finalCoord * pos finalCoord)
    