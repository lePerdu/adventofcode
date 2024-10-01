module Part2 where

import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import System.Exit

import Part1 (Input(..), Game(..), Board, getInput, makeGame, applyDraw, isBoardComplete, getScore)

findFinalBoardWin :: Input -> Maybe (NonEmpty Board, Int)
findFinalBoardWin input = go (makeGame (inputBoards input)) (inputDraws input)
    where
        go game [] = Nothing
        go game (draw:rest) =
            let Game newBoards = applyDraw draw game
                (completed, uncompleted) = partition isBoardComplete newBoards
            in case uncompleted of
                [] ->
                    -- Return last completed boards when there are no remaining
                    -- boards uncompleted
                    case completed of
                        [] -> Nothing
                        (b:bs) -> Just (b:|bs, draw)
                _ -> go (Game uncompleted) rest

main :: IO ()
main = do
    input <- getInput
    case findFinalBoardWin input of
        Nothing ->
            die "No win"
        Just ((winning :| other), finalDraw) -> do
            putStrLn $ "Last winning score: " ++ show (getScore winning finalDraw)
            when (not (null other)) $ putStrLn "(Other boards also won last)"
