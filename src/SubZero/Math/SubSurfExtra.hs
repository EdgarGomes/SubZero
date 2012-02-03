module SubZero.Math.SubSurfExtra
where

  
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM  
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set
import Maybe
  
  
type Point  = (Double, Double, Double)
data Crease = NoCrease
            | Crease
            | Corner
            deriving (Show, Eq)
data EdgeSide = RightSide
              | LeftSide
              deriving (Show, Eq)
                       
type VertexID  = Int
data Vertex    = Vertex { cord         :: Point
                        , vertexCrease :: Crease
                        , edges        :: Set EdgeID
                        } deriving (Show)
type VertexSet = IntMap Vertex

newtype EdgeID = EdgeID (VertexID, VertexID) deriving (Show) 
data Edge = Edge { edgeCrease :: Crease
                 , vertexA    :: VertexID
                 , rightFace  :: (Maybe Face)
                 , leftFace   :: (Maybe Face)
                 , vertexB    :: VertexID
                 } deriving (Show)
                 
type EdgeSet = Map EdgeID Edge

data Face = Face3 VertexID VertexID VertexID 
          | Face4 VertexID VertexID VertexID VertexID
          | FaceN [VertexID]
            deriving (Show)
                     
data Mesh = Mesh { edgeSet :: EdgeSet
                 , vertSet :: VertexSet
                 } deriving (Show)


instance Ord EdgeID where
  compare (EdgeID a) (EdgeID b) = compare a' b'
    where
      a' = sort a
      b' = sort b
      sort::Ord a => (a, a) -> (a, a)
      sort face@(a, b) = if (a >= b) then face else (b, a)

instance Eq EdgeID where
    x == y = compare x y == EQ


buildMesh::[(VertexID,Point)] -> [(VertexID, VertexID, VertexID)] -> Mesh
buildMesh vs es = foldl (addTriangle) init es
  where
    init        =  Mesh { edgeSet = Map.empty, vertSet = vsinit }
    vsinit      = foldlStrict ins IM.empty vs
    ins t (k,x) = IM.insert k (vertex x) t
    vertex x    = Vertex x NoCrease Set.empty

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}
    

--        A 
--        |
--  Left  |  Right    
--        |    
--        B   
-- For AID > BID
-- | Add a trianglelation the a mesh. The input triple is assumed to be in
--   clockwise format (a,b,c) a -> b -> c
addTriangle::Mesh -> (VertexID, VertexID, VertexID) -> Mesh
addTriangle ms (a,b,c) =  addEdge a b $ addEdge b c $ addEdge c a ms
  where
    face          = Face3 a b c
    
    addEdge x y ms =
      let
        e   = edgeID x y
        es' = Map.alter (solveCollision (buildLath x y)) e (edgeSet ms)
        vs' = addEdgeVS e y $ addEdgeVS e x (vertSet ms)
      in Mesh { vertSet = vs', edgeSet = es' }
    
    addEdgeVS e v vs  = if IM.member v vs
                        then IM.adjust (\vs -> vs { edges = Set.insert e (edges vs) } ) v vs
                        else error "[SubZero] Point not defined!"
    
    edgeID x y    = EdgeID (x, y)
    
    buildLath x y = Edge { edgeCrease = NoCrease
                         , vertexA    = x
                         , rightFace  = if x >= y then Just face else Nothing
                         , leftFace   = if x <  y then Just face else Nothing
                         , vertexB    =  y }       
    
    -- Check the match between two Laths (old and new polygons) if both exist
    solveCollision lath x = case x of
      Nothing      -> Just lath
      Just oldLath -> let
          checkconnction
            | xor oldLath lath = Just $ lath { leftFace  = leftFace  oldLath } 
            | xor lath oldLath = Just $ lath { rightFace = rightFace oldLath }
            | otherwise = error "[SubZero] Invalide mesh!"
          in checkconnction
    
    xor a b = let 
      f1 = isJust.rightFace
      f2 = isJust.leftFace
      in ((not.f1) a && f1 b) && (f2 a && (not.f2) b)
        

empty::Mesh
empty = Mesh { edgeSet = Map.empty
             , vertSet = IM.empty }

       


test_a = buildMesh [(x,(0.0,0.0,fromIntegral x)) | x <- [1..7]] [(1,6,7), (6,5,7), (5,4,7), (4,3,7), (3,2,7), (2,1,7)]
