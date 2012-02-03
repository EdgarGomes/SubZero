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
  
type Point3D = (Double, Double, Double)
type PatchID = Int

newtype PatchPos = PatchPos (Int,Int)  deriving (Show, Eq)

data Crease = NoCrease
            | Crease
            | Corner
            deriving (Show, Eq)
                      
data Patch a = TriPatch 
           { level     ::Int
           , nv11      ::Vector a
           , ne11nn    ::Vector a
           , nvnn      ::Vector a
           , nennn1    ::Vector a
           , nvn1      ::Vector a
           , nen111    ::Vector a
  
           , triMatrix ::Vector a
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
  
instance Ord EdgeID where
  compare (EdgeID a) (EdgeID b) = compare a' b'
    where
      a' = sort a
      b' = sort b
      sort::Ord a => (a, a) -> (a, a)
      sort face@(a, b) = if (a >= b) then face else (b, a)




buildMesh::[(Int, Int, Int)] -> (VertexSet, EdgeSet)
buildMesh = foldl addTriangle init
  where
    init = (IM.empty, Map.empty)


-- | Add a trianglelation the a mesh. The input triple is assumed to be in
--   clockwise format (a,b,c) a -> b -> c
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
    
    solveCollision ref x = case x of
      Nothing        -> Just $ Left ref
      Just (Left a) -> Just $ Right (a,ref)
      _              -> error "[SubZero] Invalide mesh!"


makepatch::(VertexSet, EdgeSet) -> (Int, Int, Int) -> Patch Int
makepatch (vs, es) p@(a,b,c) = undefined
  where
    getE x y w = let
      e = EdgeID (x, y)
      func x = case x of
        Right (a,b) -> return $ if a == w then a else b
        _           -> Nothing
      in Map.lookup e es >>= func
    e1 = getE a b
    e2 = getE b c
    e3 = getE c a