import Control.Applicative (Applicative (liftA2), liftA3)
import Control.Monad (forM_)
import Control.Monad.Trans.State
import Data.Foldable (Foldable (toList), find, maximumBy, traverse_)
import Data.Function (on)
import Data.Functor
import Data.List.Split
import Data.Maybe (catMaybes, mapMaybe)
import System.Exit (die)
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

data Reg = RW | RX | RY | RZ deriving (Eq, Ord)

instance Show Reg where
  show RW = "w"
  show RX = "x"
  show RY = "y"
  show RZ = "z"

data Op = OpReg !Reg | OpVal !Int

instance Show Op where
  show (OpReg r) = show r
  show (OpVal v) = show v

data Instr
  = Inp !Reg
  | Add !Reg !Op
  | Mul !Reg !Op
  | Div !Reg !Op
  | Mod !Reg !Op
  | Eql !Reg !Op
  deriving (Show)

data Expr
  = EVal !Int
  | EInput !Int
  | EInitReg !Reg
  | EAdd Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | EEql Expr Expr
  deriving (Show, Eq, Ord)

class AluValue v where
  fromLiteral :: Int -> v
  aluAdd :: v -> v -> v
  aluMul :: v -> v -> v
  aluDiv :: v -> v -> v
  aluMod :: v -> v -> v
  aluEql :: v -> v -> v

instance AluValue Int where
  fromLiteral = fromIntegral
  aluAdd = (+)
  aluMul = (*)
  aluDiv a b
    | b == 0 = error $ "Cannot divide " ++ show a ++ " " ++ show b
    | otherwise = quot a b -- Round towards 0, not away
  aluMod a b
    | a < 0 || b <= 0 = error $ "Cannot mod " ++ show a ++ " " ++ show b
    | otherwise = mod a b
  aluEql a b = if a == b then 1 else 0

sortPair :: Ord a => (a, a) -> (a, a)
sortPair p@(x, y) = if x <= y then p else (y, x)

instance AluValue Expr where
  fromLiteral = EVal
  aluAdd a b = case sortPair (a, b) of
    (EVal 0, y) -> y
    (EVal x, EVal y) -> EVal (x `aluAdd` y)
    (x, y) -> EAdd x y
  aluMul a b = case sortPair (a, b) of
    (EVal 0, _) -> EVal 0
    (EVal 1, y) -> y
    (EVal x, EVal y) -> EVal (x `aluMul` y)
    (x, y) -> EMul x y
  aluDiv a b = case sortPair (a, b) of
    (EVal 0, _) -> EVal 0
    (x, EVal 1) -> x
    (EVal x, EVal y) -> EVal (x `aluDiv` y)
    (x, y) -> EDiv x y
  aluMod a b = case sortPair (a, b) of
    (EVal 0, _) -> EVal 0
    (x, EVal 1) -> EVal 0
    (EVal x, EVal y) -> EVal (x `aluMod` y)
    (x, y) -> EMod x y
  aluEql a b = case sortPair (a, b) of
    (EVal x, EVal y) -> EVal (x `aluEql` y)
    (x, y) -> EEql x y

type Store v = (v, v, v, v)

data AluState v = AluState {aluRegisters :: !(Store v), aluInput :: [v]}
  deriving (Show)

newtype AluM v a = AluM {unAlu :: State (AluState v) a}
  deriving (Functor, Applicative, Monad)

getR :: Reg -> Store v -> v
getR RW (w, _, _, _) = w
getR RX (_, x, _, _) = x
getR RY (_, _, y, _) = y
getR RZ (_, _, _, z) = z

getReg :: Reg -> AluM v v
getReg r = AluM $ gets $ getR r . aluRegisters

setR :: Reg -> v -> Store v -> Store v
setR RW v (_, x, y, z) = (v, x, y, z)
setR RX v (w, _, y, z) = (w, v, y, z)
setR RY v (w, x, _, z) = (w, x, v, z)
setR RZ v (w, x, y, _) = (w, x, y, v)

setReg :: Reg -> v -> AluM v ()
setReg r v = AluM $ modify (\a -> a {aluRegisters = setR r v (aluRegisters a)})

getOp :: AluValue v => Op -> AluM v v
getOp (OpReg r) = getReg r
getOp (OpVal v) = pure $ fromLiteral v

runBinaryOp :: AluValue v => (v -> v -> v) -> Reg -> Op -> AluM v ()
runBinaryOp f r v = setReg r =<< liftA2 f (getReg r) (getOp v)

runInstr :: AluValue v => Instr -> AluM v ()
runInstr (Inp r) = do
  input <- AluM $ gets aluInput
  case input of
    [] -> error "No more input" -- Handle properly
    (next : rest) -> do
      AluM $ modify (\a -> a {aluInput = rest})
      setReg r next
runInstr (Add r v) = runBinaryOp aluAdd r v
runInstr (Mul r v) = runBinaryOp aluMul r v
runInstr (Div r v) = runBinaryOp aluDiv r v
runInstr (Mod r v) = runBinaryOp aluMod r v
runInstr (Eql r v) = runBinaryOp aluEql r v

runProgram :: AluValue v => [Instr] -> AluState v -> v
runProgram instrs = evalState (unAlu (traverse_ runInstr instrs *> getReg RZ))

runProgramZero :: AluValue v => [Instr] -> [v] -> v
runProgramZero instrs input = runProgram instrs (AluState (z, z, z, z) input)
  where
    z = fromLiteral 0

splitAtInputs :: [Instr] -> [[Instr]]
splitAtInputs = split (dropBlanks $ keepDelimsL $ whenElt (isInp))
  where
    isInp (Inp _) = True
    isInp _ = False

data Usage = Reset | RVal | Unused deriving (Eq)

isInitState :: [Instr] -> Reg -> Bool
isInitState prog reg = maybe True (== Reset) $ find (/= Unused) (map usage prog)
  where
    usage (Inp r) | r == reg = Reset
    usage (Mul r (OpVal 0)) | r == reg = Reset
    usage (Add _ (OpReg r)) | r == reg = RVal
    usage (Mul _ (OpReg r)) | r == reg = RVal
    usage (Div _ (OpReg r)) | r == reg = RVal
    usage (Mod _ (OpReg r)) | r == reg = RVal
    usage (Eql _ (OpReg r)) | r == reg = RVal
    usage _ = Unused

isInitStateNotZ :: [Instr] -> Bool
isInitStateNotZ prog = and [isInitState prog r | r <- [RW, RX, RY]]

trySplitProgram :: [Instr] -> Maybe [[Instr]]
trySplitProgram prog =
  let splitOnInp@(first : rest) = splitAtInputs prog
   in if all isInitStateNotZ rest
        then Just splitOnInp
        else Nothing

data DivZ = DivZ1 | DivZ26 deriving (Show, Eq)

-- | Template for the MONAD program.
-- Represents the state transition:
-- z' = z register at start of execution
-- in = input digit
-- x = (z' % 26 - monadInputEqOffset) /= in
-- yMul = 25*x + 1
-- yAdd = (in + monadInputOffset) * x
-- z = (z' / monadDivZ) * yMul + yAdd
data MonadTemplate = MonadTemplate
  { monadDivZ :: !DivZ,
    monadInputEqOffset :: !Int,
    monadInputOffset :: !Int
  }
  deriving (Show)

matchMonadTemplate :: [Instr] -> Maybe MonadTemplate
matchMonadTemplate
  [ Inp RW,
    Mul RX (OpVal 0),
    Add RX (OpReg RZ),
    Mod RX (OpVal 26),
    Div RZ (OpVal rawDivZ),
    Add RX (OpVal inputEqOffset),
    Eql RX (OpReg RW),
    Eql RX (OpVal 0),
    Mul RY (OpVal 0),
    Add RY (OpVal 25),
    Mul RY (OpReg RX),
    Add RY (OpVal 1),
    Mul RZ (OpReg RY),
    Mul RY (OpVal 0),
    Add RY (OpReg RW),
    Add RY (OpVal inputOffset),
    Mul RY (OpReg RX),
    Add RZ (OpReg RY)
    ] =
    liftA3
      MonadTemplate
      (checkDivZ rawDivZ)
      (pure inputEqOffset)
      (pure inputOffset)
    where
      checkDivZ 1 = Just DivZ1
      checkDivZ 26 = Just DivZ26
      checkDivZ _ = Nothing
matchMonadTemplate _ = Nothing

tryMatchProgramTemplate :: [Instr] -> Maybe [MonadTemplate]
tryMatchProgramTemplate = traverse matchMonadTemplate . splitAtInputs

isDigitVal :: Int -> Bool
isDigitVal n = 1 <= n && n <= 9

allDigits :: [Int]
allDigits = [1 .. 9] -- [9, 8 .. 1]

data PrevState = PrevState {prevInput :: Int, prevZ :: Int} deriving (Show, Eq)

computeInitZStates :: MonadTemplate -> Int -> [PrevState]
computeInitZStates
  ( MonadTemplate
      { monadDivZ = divZ,
        monadInputEqOffset = eqOffset,
        monadInputOffset = inOffset
      }
    )
  finalZ = case divZ of
    DivZ1 ->
      let eqBranch = finalZ
          eqBranchInput = finalZ `aluMod` 26 + eqOffset
          maybeEqBranch =
            if isDigitVal eqBranchInput
              then Just $ PrevState eqBranchInput eqBranch
              else Nothing
          maybeNeqBranch input =
            let numerator = finalZ - (input + inOffset)
                z' = numerator `aluDiv` 26
             in if numerator `mod` 26 == 0 && input /= z' `aluMod` 26 + eqOffset
                  then Just $ PrevState input z'
                  else Nothing
       in toList maybeEqBranch ++ mapMaybe maybeNeqBranch allDigits
    DivZ26 ->
      let eqBranches = [26 * finalZ + r | r <- [0 .. 25]]
          neqBranches input =
            let base = finalZ - (input + inOffset)
             in if base `mod` 26 == 0
                  then [base + r | r <- [0 .. 25]]
                  else []
       in catMaybes
            ( [ checkBranch (==) z' input
                | input <- allDigits,
                  z' <- eqBranches
              ]
                ++ [ checkBranch (/=) z' input
                     | input <- allDigits,
                       z' <- neqBranches input
                   ]
            )
    where
      checkBranch f z' input =
        if z' >= 0 && f input (z' `aluMod` 26 + eqOffset)
          then Just $ PrevState input z'
          else Nothing

data InitState = InitState {initZ :: !Int, initInput :: ![Int]}
  deriving (Show)

toAluState :: InitState -> AluState Int
toAluState (InitState z input) = AluState (0, 0, 0, z) input

cascadeInitStates :: [MonadTemplate] -> Int -> [InitState]
cascadeInitStates [] finalZ = [InitState finalZ []]
cascadeInitStates (first : rest) finalZ =
  let firstStepEndStates = cascadeInitStates rest finalZ
      recurse (InitState z input) =
        [ InitState (prevZ p) (prevInput p : input)
          | p <- computeInitZStates first z
        ]
   in concatMap recurse firstStepEndStates

parseReg :: Parser Reg
parseReg =
  choice
    [ char 'x' $> RX,
      char 'y' $> RY,
      char 'z' $> RZ,
      char 'w' $> RW
    ]

parseOp :: Parser Op
parseOp = OpReg <$> parseReg <|> OpVal <$> parseInt
  where
    parseInt = option id (char '-' $> negate) <*> (fromIntegral . read <$> many1 digit)

parseInstr :: Parser Instr
parseInstr = parseInp <|> parseBinary
  where
    parseInp = Inp <$> (string' "inp" *> skipMany1 space *> parseReg)
    parseBinaryCode =
      choice
        [ string' "add" $> Add,
          string' "mul" $> Mul,
          string' "div" $> Div,
          string' "mod" $> Mod,
          string' "eql" $> Eql
        ]
    parseBinary =
      parseBinaryCode
        <*> (skipMany1 space *> parseReg)
        <*> (skipMany1 space *> parseOp)

parseProgram :: Parser [Instr]
parseProgram = many (parseInstr <* optional newline)

readInput :: IO [Instr]
readInput = do
  c <- getContents
  case parse parseProgram "" c of
    Left err -> die (show err)
    Right res -> pure res

withAllDigits :: [Instr] -> [(Int, Int)]
withAllDigits prog = [(d, runProgramZero prog [d]) | d <- allDigits]

initRegs :: Store Expr
initRegs = (EInitReg RW, EInitReg RX, EInitReg RY, EInitReg RZ)

infInput :: [Expr]
infInput = map EInput [1 ..]

initExprState :: AluState Expr
initExprState = AluState initRegs infInput

main :: IO ()
main = do
  prog <- readInput
  templated <- case tryMatchProgramTemplate prog of
    Just s -> pure s
    Nothing -> die "Not splitable"
  forM_ templated print
  let initStates = cascadeInitStates templated 0
  let initZeroStates = filter ((== 0) . initZ) initStates
  let realResults = [(s, runProgram prog (toAluState s)) | s <- initZeroStates]
  forM_ (take 10 realResults) print
  print $ maximumBy (compare `on` initInput) initZeroStates
