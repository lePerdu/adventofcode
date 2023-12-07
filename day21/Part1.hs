module Part1 where

import Control.Monad (forM_)

data Player = Player {playerPos :: !Int, playerScore :: !Int}
  deriving (Show, Eq, Ord)

data Game d = Game
  { player1 :: Player,
    player2 :: Player,
    gameDice :: d
  }
  deriving (Show)

newtype DetermDice = DetermDice Int deriving (Show)

data GameResult = GameResult
  { resultLosing :: Int,
    resultRolls :: Int
  }
  deriving (Show)

class Dice d where
  rollCount :: d -> Int
  rollDice :: d -> (Int, d)

  roll3 :: d -> (Int, d)
  roll3 d =
    let (a, d') = rollDice d
        (b, d'') = rollDice d'
        (c, d''') = rollDice d''
     in (a + b + c, d''')

instance Dice DetermDice where
  rollCount (DetermDice d) = d
  rollDice (DetermDice d) = (d `mod` 100 + 1, DetermDice (d + 1))

newDetermDice :: DetermDice
newDetermDice = DetermDice 0

newPlayer :: Int -> Player
newPlayer start = Player {playerPos = start, playerScore = 0}

advancePlayer :: Player -> Int -> Player
advancePlayer p x =
  let newPos = (playerPos p + x - 1) `mod` 10 + 1
   in p {playerPos = newPos, playerScore = playerScore p + newPos}

resultScore :: GameResult -> Int
resultScore (GameResult l r) = l * r

playGameTurn :: Dice d => Game d -> Either GameResult (Game d)
playGameTurn (Game p1 p2 dice) =
  let (p1Roll, d') = roll3 dice
      p1' = advancePlayer p1 p1Roll
   in if playerScore p1' >= 1000
        then Left $ GameResult (playerScore p2) (rollCount d')
        else -- Swap players for next turn
          Right $ Game p2 p1' d'

playGame :: Dice d => Game d -> ([Game d], GameResult)
playGame = go []
  where
    go acc game = case playGameTurn game of
      Left result -> (acc ++ [game], result)
      Right game' -> go (acc ++ [game]) game'

readPlayerPos :: String -> Int
readPlayerPos = read . last . words

readInput :: IO (Int, Int)
readInput = do
  [p1, p2] <- map readPlayerPos . lines <$> getContents
  pure (p1, p2)

main :: IO ()
main = do
  (p1Start, p2Start) <- readInput
  let game = Game (newPlayer p1Start) (newPlayer p2Start) newDetermDice
  let (states, result) = playGame game
  forM_ states print
  putStrLn $ "Part1: " ++ show (resultScore result)
