{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Array
import Data.Char (isLower, isMark)
import Data.Graph
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Tuple (swap)
import System.Exit (die)

type NodeName = Text

newtype CaveNode = CaveNode {nodeName :: NodeName} deriving (Eq)

data Cave = Cave
  { caveGraph :: Graph,
    caveNodeFromVertex :: Vertex -> (CaveNode, NodeName, [NodeName]),
    caveVertexFromKey :: NodeName -> Maybe Vertex
  }

type Connection = (NodeName, NodeName)

type Path = [CaveNode]

instance Show CaveNode where
  show = show . nodeName

instance Show Cave where
  show = show . caveGraph

buildCave :: [Connection] -> Cave
buildCave conns =
  let insertAdjacency (src, dst) =
        Map.alter (\existing -> Just $ dst : fromMaybe [] existing) src
      -- Include both directions to make it an undirected graph
      forwardAndReverseConns = conns ++ map swap conns
      -- Mapping from node -> connected nodes
      edgesMap = foldr insertAdjacency Map.empty forwardAndReverseConns
      convertToNode (src, dsts) = (CaveNode src, src, dsts)
      edges = map convertToNode $ Map.toList edgesMap
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges edges
   in Cave graph nodeFromVertex vertexFromKey

vertexNode cave v = let (node, _, _) = caveNodeFromVertex cave v in node

class VisitConstraints c where
  canVisit :: c -> Vertex -> Bool
  markVisit :: c -> Vertex -> c

newtype SingleVisit = SingleVisit IntSet.IntSet deriving (Show)

instance VisitConstraints SingleVisit where
  canVisit (SingleVisit visited) vertex = IntSet.notMember vertex visited
  markVisit s@(SingleVisit visited) vertex
    | canVisit s vertex = SingleVisit $ IntSet.insert vertex visited
    | otherwise = error $ "Already visited " ++ show s ++ ", " ++ show vertex

makeSingleVisit startVertex = SingleVisit $ IntSet.singleton startVertex

data VisitTwice = VisitTwice SingleVisit Vertex Bool deriving (Show)

instance VisitConstraints VisitTwice where
  canVisit (VisitTwice single start twice) vertex =
    canVisit single vertex || (not twice && vertex /= start)
  markVisit v@(VisitTwice single start twice) vertex
    | canVisit single vertex = VisitTwice (markVisit single vertex) start twice
    | not twice = VisitTwice single start True
    | otherwise = error $ "Already visited " ++ show v ++ ", " ++ show vertex

makeVisitTwice :: Vertex -> VisitTwice
makeVisitTwice start = VisitTwice (makeSingleVisit start) start False

findPaths :: VisitConstraints c => Cave -> (Vertex -> c) -> NodeName -> NodeName -> [Path]
findPaths cave mkCons start end =
  map convertPath $ search (mkCons startVertex) [startVertex]
  where
    Just startVertex = caveVertexFromKey cave start
    Just endVertex = caveVertexFromKey cave end

    search visited path@(headVertex : _)
      | headVertex == endVertex = [path]
      | otherwise =
          let adjacent = caveGraph cave ! headVertex
              possibleAdjacent = filter (bigOrCanVisit visited) adjacent
              subSearch v = search (bigOrMarkVisit visited v) (v : path)
           in concatMap subSearch possibleAdjacent

    isSmall = Text.all isLower . nodeName . vertexNode cave

    bigOrCanVisit visited v =
      not (isSmall v) || canVisit visited v

    bigOrMarkVisit visited v
      | isSmall v = markVisit visited v
      | otherwise = visited

    -- Convert backward list of vertices to forward list of node names
    convertPath = map (vertexNode cave) . reverse

parseLine :: Text -> Maybe Connection
parseLine line = case Text.split (== '-') line of
  [src, dst] -> Just (src, dst)
  _ -> Nothing

readInput :: IO [Connection]
readInput = do
  contents <- Text.IO.getContents
  case traverse parseLine $ Text.lines contents of
    Just conns -> pure conns
    Nothing -> die "Invalid input"

main :: IO ()
main = do
  connections <- readInput
  let cave = buildCave connections
  let paths = findPaths cave makeSingleVisit "start" "end"
  -- forM_ paths print
  let pathCount = length paths
  putStrLn $ "Part1: " ++ show pathCount

  let paths = findPaths cave makeVisitTwice "start" "end"
  -- forM_ paths print
  let pathCount = length paths
  putStrLn $ "Part2: " ++ show pathCount
