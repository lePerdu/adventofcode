module Part2 where

import Control.Monad (forM_)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Part1 (Player (..), advancePlayer, newPlayer, readInput)

data GameState = GameState {player1 :: !Player, player2 :: !Player}
  deriving (Show, Eq, Ord)

data RoundResult = P1Wins | P2Wins | InProgress GameState
  deriving (Show, Eq, Ord)

newtype UGames = UGames {gamesInProgress :: Map RoundResult Integer}
  deriving (Show)

newUGames :: Player -> Player -> UGames
newUGames p1 p2 =
  UGames {gamesInProgress = Map.singleton (InProgress $ GameState p1 p2) 1}

diceRolls :: [Int]
diceRolls = [a + b + c | a <- roll, b <- roll, c <- roll]
  where
    roll = [1, 2, 3]

diceRollPairs :: [(Int, Int)]
diceRollPairs = [(x, y) | x <- diceRolls, y <- diceRolls]

hasWon :: Player -> Bool
hasWon p = playerScore p >= 21

runRound :: RoundResult -> [RoundResult]
runRound (InProgress (GameState p1 p2)) = concatMap runP2Rolls (allBranches p1)
  where
    allBranches p = [advancePlayer p r | r <- diceRolls]
    runP2Rolls p1'
      | playerScore p1' >= 21 = [P1Wins]
      | otherwise = map (finalState p1') (allBranches p2)
    finalState p1' p2'
      | playerScore p2' >= 21 = P2Wins
      | otherwise = InProgress (GameState p1' p2')
runRound r = [r]

nextGameStates :: UGames -> UGames
nextGameStates (UGames states) =
  UGames $
    Map.fromListWith (+) $
      concatMap advanceState $
        Map.toList states
  where
    advanceState (state, count) = [(r, count) | r <- runRound state]

allWon :: UGames -> Bool
allWon (UGames states) = all isWinState $ Map.keys states
  where
    isWinState P1Wins = True
    isWinState P2Wins = True
    isWinState _ = False

findFinalState :: [UGames] -> UGames
findFinalState = fromJust . find allWon

getMinScore :: UGames -> Int
getMinScore (UGames states) = minimum $ concatMap getScores $ Map.keys states
  where
    getScores (InProgress (GameState p1 p2)) = [playerScore p1, playerScore p2]
    getScores _ = []

getMostWins :: UGames -> Integer
getMostWins (UGames states) = max (states Map.! P1Wins) (states Map.! P2Wins)

main :: IO ()
main = do
  (p1Start, p2Start) <- readInput
  let games = newUGames (newPlayer p1Start) (newPlayer p2Start)
  let states = iterate nextGameStates games
  let minScores = map getMinScore states
  forM_ (take 5 minScores) $ \s -> do
    print s

  let final = findFinalState states

  let ans = getMostWins final
  putStrLn $ "Part2: " ++ show ans
