module SubZero.Math.SubSurf
where

  
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM  
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vec
  
type Point3D = (Double, Double, Double)
type PatchID = Int

newtype PatchPos = PatchPos (Int,Int)  deriving (Show, Eq)

data VertexConn a = OnRight a
                  | OnLeft a
                  | BothLR a a
                  | JustOne a
                  | None
                  deriving (Show, Eq)
                           
data Crease = NoCrease
            | Crease
            | Corner
            deriving (Show, Eq)

-- | The Patch data defines the central triangular patch (a triangular matrix)
-- and the first neighbours for each vertex and edge.                      
-- 
--         (1, 1)
--           /\ 
--          /  \
--         /    \
--      (n,1)__(n,n)
data Patch a = TriPatch 
           { level     ::Int                   -- ^ Level of subdivision
           , nv11      ::VertexConn (Vector a) -- ^ Neighbors of vertex (1,1)
           , ne11nn    ::Maybe (Vector a)      -- ^ Neighbors of edge (1,1) -- (n,n)
           , nvnn      ::VertexConn (Vector a) -- ^ Neighbors of vertex (n,n)
           , nennn1    ::Maybe (Vector a)      -- ^ Neighbors of edge (n,n) -- (n,1) 
           , nvn1      ::VertexConn (Vector a) -- ^ Neighbors of vertex (n,1)
           , nen111    ::Maybe (Vector a)      -- ^ Neighbors of edge (n,1) -- (1,1)
  
           , triMatrix ::Vector a -- ^ Triangular matrix
           } deriving (Show, Eq)                     
                      
type Mesh = Vector (Patch Int)

sn::Int -> Int
sn n = n*(1+n) `div` 2

ix::PatchPos -> Int
ix (PatchPos (i,j)) = sn (j-1) + i 
                                   
pos::Int -> PatchPos
pos ix = let (j, total) = func 0 in PatchPos (ix - total, j)
  where
    func n
      | sn (n+1) > ix = (n, sn n)
      | otherwise     = func (n+1)




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
makepatch::(VertexSet, EdgeSet) -> (Int, Int, Int) -> Patch Int
makepatch (vs, es) p@(a,b,c) = patch
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
      _         -> None

    patch = TriPatch { level     = 0
                     , nv11      = toVec v1
                     , ne11nn    = fmap Vec.singleton e12
                     , nvnn      = toVec v2
                     , nennn1    = fmap Vec.singleton e23
                     , nvn1      = toVec v3
                     , nen111    = fmap Vec.singleton e31
                     , triMatrix = Vec.fromList [a,b,c] }



-- test case
testMesh = buildMesh [(1,5,2),(5,6,2),(5,8,6),(6,8,11),(6,11,13),(3,13,11)
                     ,(13,3,12),(10,13,12),(7,10,12),(12,4,7),(4,12,9)
                     ,(2,10,7),(2,7,1),(7,15,14),(1,7,14),(7,4,15)]