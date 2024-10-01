{-# LANGUAGE BlockArguments #-}

import Data.Bits (Bits, FiniteBits)
import Data.Char (isSpace, ord)
import Data.Foldable (Foldable (foldl'))
import Data.Word
import System.Exit (die)
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

data LengthType = LengthInBits Word16 | LengthInPackets Word16 deriving (Show)

data OpType = SumOp | MulOp | MinOp | MaxOp | LTOp | GTOp | EQOp deriving (Show)

data PacketData
  = LiteralPacket Integer
  | OperatorPacket
      { operatorType :: OpType,
        operatorLength :: LengthType,
        operatorSubPackets :: [Packet]
      }
  deriving (Show)

type Version = Word8

data Packet = Packet {packetVersion :: Version, packetData :: PacketData}
  deriving (Show)

type Parser = Parsec [Bool] ()

showBit :: Bool -> String
showBit True = "1"
showBit False = "0"

satisfy :: Stream s m Bool => (Bool -> Bool) -> ParsecT s u m Bool
satisfy f =
  tokenPrim
    showBit
    (\p _ _ -> incSourceColumn p 1)
    (\b -> if f b then Just b else Nothing)

anyBit :: Stream s m Bool => ParsecT s u m Bool
anyBit = satisfy (const True)

exactBit :: Stream s m Bool => Bool -> ParsecT s u m Bool
exactBit b = satisfy (== b)

exactBits :: Stream s m Bool => [Bool] -> ParsecT s u m [Bool]
exactBits = tokens (concatMap showBit) (\p ts -> incSourceColumn p (length ts))

bitsToNum :: (Bits n, Integral n) => [Bool] -> n
bitsToNum = foldl' (\acc b -> 2 * acc + if b then 1 else 0) 0

numToBits :: (Bits n, Integral n) => n -> [Bool]
numToBits = go []
  where
    go [] 0 = [False]
    go acc 0 = acc
    go _ n | n < 0 = error "numToBits only converts positive numbers"
    go acc n =
      let (n', digit) = n `divMod` 2
          digitBit = digit == 1
       in go (digitBit : acc) n'

padLeft :: Int -> a -> [a] -> [a]
padLeft width pad arr =
  let padding = width - length arr
   in if padding <= 0
        then arr
        else replicate padding pad ++ arr

fixedWidthNum ::
  (Stream s m Bool, FiniteBits n, Integral n) => Int -> ParsecT s u m n
fixedWidthNum n = bitsToNum <$> count n anyBit

parseLiteralPacketValue :: Parser Integer
parseLiteralPacketValue = do
  segments <- many literalSegment
  lastSegment <- endSegment
  let allBits = concat $ segments ++ [lastSegment]
  let value = bitsToNum allBits
  pure value
  where
    nibble = count 4 anyBit
    literalSegment = exactBit True *> nibble
    endSegment = exactBit False *> nibble

parseOpPacket :: Word8 -> Parser PacketData
parseOpPacket typeId = do
  opType <- parseOpType typeId
  len <- inBits <|> inPackets
  subPackets <- case len of
    LengthInBits bitCount -> do
      packetContents <- count (fromIntegral bitCount) anyBit
      sourceName <- sourceName <$> getPosition
      case parse fullPackets (sourceName ++ " sub-packet") packetContents of
        Left err -> fail $ show err
        Right packets -> pure packets
    LengthInPackets packetCount -> count (fromIntegral packetCount) parsePacket
  pure $ OperatorPacket opType len subPackets
  where
    inBits = exactBit False *> (LengthInBits <$> fixedWidthNum 15)
    inPackets = exactBit True *> (LengthInPackets <$> fixedWidthNum 11)

    fullPackets = many parsePacket <* eof

    parseOpType typeId = case typeId of
      0 -> pure SumOp
      1 -> pure MulOp
      2 -> pure MinOp
      3 -> pure MaxOp
      5 -> pure GTOp
      6 -> pure LTOp
      7 -> pure EQOp
      _ -> fail $ "Invalid operator type: " ++ show typeId

parsePacketData :: Parser PacketData
parsePacketData = do
  typeId <- fixedWidthNum 3
  case typeId of
    4 -> LiteralPacket <$> parseLiteralPacketValue
    _ -> parseOpPacket typeId

parsePacket :: Parser Packet
parsePacket = Packet <$> fixedWidthNum 3 <*> parsePacketData

-- | Parse a packet with optional zero padding at the end
parseTopLevelPacket :: Parser Packet
parseTopLevelPacket = parsePacket <* skipMany (exactBit False) <* eof

hexDigitToBits :: Char -> Either String [Bool]
hexDigitToBits char = padLeft 4 False . numToBits <$> digitValue
  where
    digitValue
      | '0' <= char && char <= '9' = Right $ ord char - ord '0'
      | 'A' <= char && char <= 'F' = Right $ ord char - ord 'A' + 10
      | otherwise = Left $ "Invalid character: " ++ show char

readInput :: IO [Bool]
readInput = do
  hexString <- getContents
  case traverse hexDigitToBits (filter (not . isSpace) hexString) of
    Left err -> die err
    Right bitGroups -> pure $ concat bitGroups

collectVersions :: Packet -> [Version]
collectVersions (Packet {packetVersion, packetData}) =
  packetVersion
    : ( case packetData of
          LiteralPacket _ -> []
          OperatorPacket {operatorSubPackets = ps} ->
            concatMap collectVersions ps
      )

sumVersions :: [Version] -> Int
sumVersions = sum . map fromIntegral

data EvalError = NotEnoughArgs Packet | TooManyArgs Packet deriving (Show)

evalPacket :: Packet -> Either EvalError Integer
evalPacket packet@(Packet _ packetData) = case packetData of
  LiteralPacket v -> Right v
  OperatorPacket opType _ args -> do
    evalArgs <- traverse evalPacket args
    let basicOp f = Right $ f evalArgs
    let binaryOp = mkBinaryOp evalArgs
    case opType of
      SumOp -> basicOp sum
      MulOp -> basicOp product
      MinOp -> basicOp minimum
      MaxOp -> basicOp maximum
      GTOp -> binaryOp (>)
      LTOp -> binaryOp (<)
      EQOp -> binaryOp (==)
  where
    mkBinaryOp args f = case args of
      [a, b] -> Right $ fromIntegral $ fromEnum $ f a b
      (_ : _ : _ : _rest) -> Left $ TooManyArgs packet
      _ -> Left $ NotEnoughArgs packet

main :: IO ()
main = do
  dataBits <- readInput
  -- putStrLn $ concatMap showBit dataBits
  packet <- case parse parseTopLevelPacket "dataBits" dataBits of
    Left err -> die $ show err
    Right p -> pure p
  print packet
  let versionSum = sumVersions $ collectVersions packet
  putStrLn $ "Part1: " ++ show versionSum

  case evalPacket packet of
    Left err -> die $ show err
    Right ans -> putStrLn $ "Part2: " ++ show ans
