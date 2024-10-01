module Part2 where

import Part1 (Command(..), input)

data SubState = SubState { depth :: Int, horizontal :: Int, aim :: Int } deriving (Show)

applyCommand :: Command -> SubState -> SubState
applyCommand command (SubState { depth, horizontal, aim }) = case command of
    Forward x -> SubState {
            depth = depth + aim * x, horizontal = horizontal + x, aim
        }
    Up x -> SubState { depth, horizontal, aim = aim - x }
    Down x -> SubState { depth, horizontal, aim = aim + x }

foldCommands :: SubState -> [Command] -> SubState
foldCommands initState [] = initState
foldCommands initState (c:rest) = foldCommands (applyCommand c initState) rest

main :: IO ()
main = do
    commands <- input
    let finalState = foldCommands (SubState 0 0 0) commands
    putStrLn $ "Final state: " ++ show finalState
    putStrLn $ "Part2: " ++ show (depth finalState * horizontal finalState)