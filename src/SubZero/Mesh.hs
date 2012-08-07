{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SubZero.Mesh
where

  
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM  
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Vector (Vector, (!), ifoldl')
import qualified Data.Vector as Vec

import Control.Monad

import Hammer.Math.Vector hiding (Vector)
import SubZero.Base


newtype EdgeID = EdgeID (Int, Int) deriving (Show, Eq)
type EdgeInfo  = Either Int (Int, Int)
type EdgeSet   = Map EdgeID EdgeInfo
type VertexSet = IntMap IntSet
  
-- | Allows ordering of EdgeID, such (a,b) = (b,a)                 
instance Ord EdgeID where
  compare (EdgeID a) (EdgeID b) = compare a' b'
    where
      a' = sort a
      b' = sort b
      sort::Ord a => (a, a) -> (a, a)
      sort face@(a, b) = if (a >= b) then face else (b, a)

-- | Create a set of connections for vertex and edges.
--        i            v       
--       /             |
-- t -- v -- j    j <- | -> t
--       \             |
--        k            i
buildMesh::[(Int, Int, Int)] -> (VertexSet, EdgeSet)
buildMesh = foldl addTriangle init
  where
    init = (IM.empty, Map.empty)

-- | Add a trianglulation from the mesh.
addTriangle::(VertexSet, EdgeSet) -> (Int, Int, Int) -> (VertexSet, EdgeSet)
addTriangle (vs, es) t@(a,b,c) = (vs', es')
  where
    edgeID x y = EdgeID (x, y)
    vs' = addVertex a b c $ addVertex b c a $ addVertex c a b vs
    es' = addEdge a b c $ addEdge b c a $ addEdge c a b es

    addEdge x y ref ms =
      let e = edgeID x y
      in Map.alter (solveCollision ref) e ms
    
    addVertex v x y vs = let ins x y = IS.insert x . IS.insert y in
      if IM.member v vs
      then IM.adjust (\vs -> ins x y vs ) v vs
      else IM.insert v (ins x y IS.empty) vs
    
    -- A 2D surface edge can be assigned to max only 2 members (e.g. triangles) 
    solveCollision ref x = case x of
      Nothing       -> Just $ Left ref
      Just (Left a) -> if a /= ref then Just $ Right (a,ref) else error "[SubZero] Duplicated patch"
      _             -> error "[SubZero] Invalide mesh!"


-- | Create a Patch (with its neighbour vertexs and edges) and
-- regarding the sequence of its neighbours according to the clockwise direction.
-- OBS: - It doesn't work for mesh with holes
--      - It doesn't consider disjoints by one vertex (e.g. |><|)
makepatch::(VertexSet, EdgeSet) -> [Int] -> (Int, Int, Int) -> Patch Int
makepatch (vs, es) creases p@(a,b,c) = patch
  where
    isMember x = x == a || x == b || x == c
    
    -- get the opposite member of the edge, if it is the case.
    getE x y isMember = let
      eID = EdgeID (x, y)
      func x = case x of
        Right (e1,e2) -> return $ if isMember e1 then e2 else e1        
        Left e        -> Nothing
      in Map.lookup eID es >>= func
    
    -- get a proper seq of vertex neighbours (execpet its own members and edge neighbours)
    getV v vref = let
      func prev next = case getE v next (== prev) of 
        Just n -> if isMember n then [] else next:(func next n)
        _      -> [next]
      in getE v vref isMember >>= return . func vref
    
    e12 = getE a b isMember
    e23 = getE b c isMember
    e31 = getE c a isMember
    
    -- check for disconnection on the vertex neighbour
    checkSplit el v er = case (el >>= getV v,  er >>= getV v) of
      (Just left, Just right) -> if left == reverse right
                                 then JustOne left
                                 else BothLR left right
      (Just x,       Nothing) -> OnLeft x
      (Nothing,       Just x) -> OnRight $ reverse x
      _                       -> None

    v1 = checkSplit e31 a e12
    v2 = checkSplit e12 b e23
    v3 = checkSplit e23 c e31
    
    toVec v = case v of
      OnLeft  x  -> OnLeft  $ Vec.fromList x
      OnRight x  -> OnRight $ Vec.fromList x
      BothLR a b -> BothLR (Vec.fromList a) (Vec.fromList b)
      JustOne x  -> JustOne $ Vec.fromList x
      _          -> None

    isCrease x = if x `elem` creases then Corner else Smooth

    patch = TriPatch { level     = 0
                                   
                     , nv00      = toVec v1
                     , v00Type   = isCrease a
                                                                               
                     , ne00nn    = fmap Vec.singleton e12
                                                                  
                     , nvnn      = toVec v2
                     , vnnType   = isCrease b
                                                                               
                     , nennn0    = fmap Vec.singleton e23
                                                                 
                     , nvn0      = toVec v3
                     , vn0Type   = isCrease c
                                                                               
                     , nen000    = fmap Vec.singleton e31
                                                                 
                     , triMatrix = Vec.fromList [a,c,b] }



