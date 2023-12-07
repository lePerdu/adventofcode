import Data.Char (isSpace)
import Data.Foldable (Foldable (foldl'), forM_, traverse_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Tuple (swap)
import System.Exit (die)

data SyntaxErrorType
  = Unclosed (NonEmpty BraceType)
  | Unopened BraceType
  | WrongClose {wrongExpected :: BraceType, wrongFound :: BraceType}
  deriving (Eq, Show)

type Pos = Int

data SyntaxError = SyntaxError
  { errorType :: SyntaxErrorType,
    errorPos :: Pos
  }
  deriving (Show)

data BraceType = Paren | Square | Curly | Angle deriving (Eq, Show)

data BraceSide = Open | Close deriving (Eq, Show)

data Brace = Brace BraceSide BraceType deriving (Eq)

newtype InputError = InvalidChar Char deriving (Show)

newtype Line = Line [Brace]

type Input = [Line]

checkSyntax :: Line -> Maybe SyntaxError
checkSyntax (Line bs) = go 0 [] bs
  where
    mkErr pos errType = Just $ SyntaxError errType pos
    go :: Pos -> [BraceType] -> [Brace] -> Maybe SyntaxError
    -- Want to end with no remaining braces left incomplete
    go _ [] [] = Nothing
    go pos (unclosed : rest) [] = mkErr pos (Unclosed (unclosed :| rest))
    -- Can always take an opening brace
    go pos unclosed (Brace Open braceType : rest) = go (pos + 1) (braceType : unclosed) rest
    go pos [] (Brace Close braceType : _) = mkErr pos (Unopened braceType)
    -- Make sure close brace matches expected
    go pos (nextUnclosed : restUnclosed) (Brace Close braceType : restBraces)
      | nextUnclosed == braceType = go (pos + 1) restUnclosed restBraces
      | otherwise = mkErr pos (WrongClose nextUnclosed braceType)

isCorrupted :: SyntaxErrorType -> Bool
isCorrupted (WrongClose _ _) = True
isCorrupted (Unopened _) = True
isCorrupted _ = False

syntaxErrorScore :: SyntaxErrorType -> Int
syntaxErrorScore = braceTypeScore . illegalChar
  where
    braceTypeScore Paren = 3
    braceTypeScore Square = 57
    braceTypeScore Curly = 1197
    braceTypeScore Angle = 25137

    illegalChar (Unclosed (t :| _)) = t -- Just take the first
    illegalChar (Unopened t) = t
    illegalChar (WrongClose _ t) = t

maybeFixable :: SyntaxError -> Maybe [BraceType]
maybeFixable (SyntaxError (Unclosed types) _) = Just $ toList types
maybeFixable _ = Nothing

-- | Find braces to fix an Unclosed syntax error
makeSyntaxFix :: [BraceType] -> Line
makeSyntaxFix = Line . map (Brace Close)

syntaxFixScore :: [BraceType] -> Int
syntaxFixScore = foldl' (\acc b -> 5 * acc + braceTypeScore b) 0
  where
    braceTypeScore Paren = 1
    braceTypeScore Square = 2
    braceTypeScore Curly = 3
    braceTypeScore Angle = 4

braceCharMapping :: [(Char, Brace)]
braceCharMapping =
  [ ('(', Brace Open Paren),
    (')', Brace Close Paren),
    ('[', Brace Open Square),
    (']', Brace Close Square),
    ('{', Brace Open Curly),
    ('}', Brace Close Curly),
    ('<', Brace Open Angle),
    ('>', Brace Close Angle)
  ]

braceToChar :: Brace -> Char
braceToChar b = fromJust $ lookup b (map swap braceCharMapping)

instance Show Brace where
  -- Brace is guaranteed to be in lookup table
  show b = [braceToChar b]

instance Show Line where
  show (Line bs) = map braceToChar bs

lexBraces :: String -> Either InputError Line
lexBraces = fmap Line . traverse lexChar . filter (not . isSpace)
  where
    lexChar c = maybe (Left (InvalidChar c)) Right $ lookup c braceCharMapping

readInput :: IO Input
readInput = do
  contents <- getContents
  case traverse lexBraces (lines contents) of
    Right result -> pure result
    Left err -> die $ show err

main :: IO ()
main = do
  input <- readInput
  let checkResults = map (\line -> (line, checkSyntax line)) input
  forM_ checkResults $ \(line, check) -> do
    putStrLn $ show line ++ " : " ++ show check
  let corrupted = filter isCorrupted $ map errorType $ mapMaybe snd checkResults
  let corruptedScore = sum $ map syntaxErrorScore corrupted
  putStrLn $ "Part1: " ++ show corruptedScore
  let fixes = mapMaybe (\(line, err) -> (,) line <$> (err >>= maybeFixable)) checkResults
  forM_ fixes $ \(line, fix) -> do
    putStrLn $ show line ++ " : " ++ show (makeSyntaxFix fix)
  let fixScores = map (syntaxFixScore . snd) fixes
  let middle = length fixScores `div` 2
  let finalScore = sort fixScores !! middle
  putStrLn $ "Part1: " ++ show finalScore
