module Part1 where

import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import System.Exit
import Text.Read
import Text.ParserCombinators.ReadP

type InitBoard = [[Int]]

data Input = Input {
    inputDraws :: [Int],
    inputBoards :: [InitBoard]
} deriving (Show)

data Cell = Cell { cellNumber :: Int, cellMarked :: Bool } deriving (Show)

type Board = [[Cell]]

newtype Game = Game [Board] deriving (Show)

data GameResult = NoWin | Win (NonEmpty Board) Int deriving (Show)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = foldr (\row acc -> map f row : acc) [] where

makeGame :: [InitBoard] -> Game
makeGame initBoards = Game $ map (map2d (\v -> Cell v False)) initBoards

markCell :: Int -> Cell -> Cell
markCell value cell =
    if cellNumber cell == value
    then cell { cellMarked = True }
    else cell

markBoard :: Int -> Board -> Board
markBoard value = map2d (markCell value)

isBoardComplete :: Board -> Bool
isBoardComplete board = isRowComplete board || isRowComplete (transpose board)
    where
        isRowComplete = any (all cellMarked)

getScore :: Board -> Int -> Int
getScore board finalDraw =
    let unmarked = filter (not . cellMarked) $ concat board
        unmarkedSum = sum $ map cellNumber unmarked
    in unmarkedSum * finalDraw

applyDraw :: Int -> Game -> Game
applyDraw drawValue (Game boards) = Game $ map (markBoard drawValue) boards

playGame :: Input -> (GameResult, Game)
playGame input = go (makeGame (inputBoards input)) (inputDraws input)
    where
        go game [] = (NoWin, game)
        go game (draw:rest) =
            let nextGame@(Game newBoards) = applyDraw draw game
                completedBoards = filter isBoardComplete newBoards
            in case completedBoards of
                [] -> go nextGame rest
                (b:bs) -> (Win (b :| bs) draw, nextGame)

boardSize :: Int = 5

nextLine :: ReadP () = char '\n' *> skipSpaces

parseInt :: ReadP Int
parseInt = readS_to_P reads

parseRow :: ReadP [Int]
parseRow = count boardSize parseInt

parseBoard :: ReadP InitBoard
parseBoard = count boardSize (parseRow <* nextLine)

parseDraws :: ReadP [Int]
parseDraws = sepBy1 parseInt (char ',')

parseInput :: ReadP Input
parseInput = do
    draws <- parseDraws
    nextLine
    boards <- many1 parseBoard
    eof
    pure $ Input draws boards


getInput :: IO Input
getInput = do
    text <- getContents
    let parse = readP_to_S parseInput text
    case parse of
        [(result, _)] -> pure result
        [] -> die "Input does not match"
        _ -> die "Input matches more than 1 way"

main :: IO ()
main = do
    input <- getInput
    let (result, game) = playGame input
    case result of
        NoWin ->
            print game >> die "No win"
        Win (winning :| other) finalDraw -> do
            putStrLn $ "Winning score: " ++ show (getScore winning finalDraw)
            when (not (null other)) $ putStrLn "(Other boards also won)"
