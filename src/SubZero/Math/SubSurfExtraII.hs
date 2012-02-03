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
  
  
type Point3D  = (Double, Double, Double)
data Crease = NoCrease
            | Crease
            | Corner
            deriving (Show, Eq)

data LathSide = RightSide
              | LeftSide
              deriving (Show, Eq)
                       
type VertexID   = Int
type VertexConn = IntMap IntSet
type VertexSet  = Vector Point3D

data Face = Face3 VertexID VertexID VertexID (Maybe VertexID) (Maybe VertexID) (Maybe VertexID)
          | FaceN [(VertexID, VertexID, Maybe VertexID)]
            deriving (Show)

data Mesh = Mesh { faceList :: [Face]
                 , vertSet  :: VertexSet
                 , connSet  :: VertexConn
                 }


buildMesh::[(VertexID, VertexID, VertexID)] -> LathSet         
buildMesh = foldl disassemble Map.empty

type EdgeSet = Map (VertexID, VertexID) (Maybe VertexID, Maybe VertexID) 

disassemble::(VertexID, VertexID, VertexID) -> EdgeSet -> EdgeSet
disassemble (a,b,c) em = fold add em [((a,b), c), ((b,c), a), ((c,a), b)]     
  where
    add (k,v) m = let edge = (Just v, Nothing) in Map.insertWith func k edge m
    func new old = case old of
      (Just x, Nothing) -> (Just x, fst $ new)
      _                 -> error "SubZero: Improper mesh!" 
             
-- | Add a trianglelation the a mesh. The input triple is assumed to be in
--   clockwise format (a,b,c) a -> b -> c
addTriangle::LathSet -> (VertexID, VertexID, VertexID) -> LathSet
addTriangle ls (a,b,c) =  addLath a b c $ addLath b c a $ addLath c a b ls
  where
    addLath x y z    = Map.alter (solveCollision (buildLath x y z)) (lathID x y)
    lathID x y       = LathID (x, y)
    buildLath x y z  = Lath { edgeCrease = NoCrease
                            , vertexA    = x
                            , nextCW_A   = Just $ lathID z x
                            , nextACW_A  = Nothing
                            , nextCW_B   = Nothing
                            , nextACW_B  = Just $ lathID z y
                            , vertexB    =  y }       
    -- Check the match between two Laths (old and new polygons) if both exist
    solveCollision lath x = case x of
      Nothing      -> Just lath
      Just oldLath -> checkconnction
        where
          checkconnction
            | (vertexA lath) == (vertexA oldLath) && xorLath lath oldLath =
              Just $ oldLath { nextCW_B  = nextCW_B lath
                             , nextACW_A = nextACW_A lath } 
            | (vertexA lath) /= (vertexA oldLath) && xnorLath lath oldLath =
              Just $ oldLath { nextCW_B  = nextCW_A lath
                             , nextACW_A = nextACW_B lath }
            | otherwise = error "[SubZero] Invalide mesh!" 


-- | Applies XNOR logic (coincidence) between the links of two Laths.
-- See the diagram bellow how match the four possible Lath combinations.
-- for (cw_A, acw_A, cw_B, acw_B) where N is Nothing and J is Just                 
--  
--        A      B              B      A
--        |      |              |      |
--   cw_A-|-    -|-acw_B   cw_B-|-    -|-acw_A
--  acw_B-|-    -|-cw_A   acw_A-|-    -|-cw_B
--        |      |              |      |
--        B      A              A      B
--     (J _, N, N, J _)      (N, J _, J _, N)
--     (J _, N, N, J _)      (N, J _, J _, N)          
xnorLath::Lath -> Lath -> Bool
xnorLath a b = func nextCW_A && func nextACW_A && func nextCW_B && func nextACW_B
  where
  func g = not $ xor (isJust.g $ a) (isJust.g $ b)          


-- | Applies XOR logic (exclusive) between the links of two Laths.
-- See the diagram bellow how match the four possible Lath combinations
-- for (cw_A, acw_A, cw_B, acw_B) where N is Nothing and J is Just
--
--        A      A              B      B
--        |      |              |      |
--   cw_A-|-    -|-acw_A   cw_B-|-    -|-acw_B
--  acw_B-|-    -|-cw_B   acw_A-|-    -|-cw_A
--        |      |              |      |
--        B      B              A      A
--     (J _, N, N, J _)      (N, J _, J _, N)        
--     (N, J _, J _, N)      (J _, N, N, J _)   
xorLath::Lath -> Lath -> Bool
xorLath a b = func nextCW_A && func nextACW_A && func nextCW_B && func nextACW_B
  where
  func g = xor (isJust.g $ a) (isJust.g $ b)


-- | The XOR is a digital logic that implements an exclusive disjunction.
--   <True False> and <False True> returns <True> otherwise <False>.
xor::Bool -> Bool -> Bool
xor a b = ((not a) && b) || (a && (not b))
        

empty::Mesh
empty = Mesh { lathSet = Map.empty
             , vertSet = IM.empty }

withLaths::(LathSet -> LathSet) -> Mesh -> Mesh
withLaths f mesh = mesh { lathSet = (f . lathSet) mesh}

nextNeigbour::LathSet -> (Maybe LathID, Maybe LathID) -> VertexID -> (Maybe LathID, Maybe LathID) 
nextNeigbour ls (lcw, lacw) id = (lcw >>= getLath >>= nextCW, lacw >>= getLath >>= nextACW)
  where
    nextCW  l = if id == (vertexA l) then nextCW_A l else nextCW_B l
    nextACW l = if id == (vertexA l) then nextACW_A l else nextACW_B l
    getLath x = Map.lookup x ls




    
scanAllNeighbour::LathSet -> VertexID -> VertexID -> [VertexID]
scanAllNeighbour ls center start =
  case get fst of
    [_] -> []
    x   -> x
  where
    guess                = Just $ LathID (start,center)
    fst                  = nextNeigbour ls (guess, guess) center
    takeN (LathID (a,b)) = if a == center then b else a
    get pair             =
      let newPair = nextNeigbour ls pair center
      in case pair of
        (Just a, Just b)   -> if a == b then [takeN a, start] else (takeN a):(takeN b):(get newPair) 
        (Just a, Nothing)  -> (takeN a):(get newPair)
        (Nothing, Just a)  -> (takeN a):(get newPair)
        (Nothing, Nothing) -> [start]



--     cw A acw
--       \|/
--  Left  |  Right    
--       /|\    
--    acw B cw   
-- For AID > BID 
nextOnFace::LathSet -> LathID -> LathID -> (VertexID, Maybe LathID)
nextOnFace ls lath prevLath = Map.lookup lath ls >>= next
  where
    next l
      | nextACW_A l == Just prevLath = (vertexB l, nextCW_B l)
      | nextCW_A l  == Just prevLath = (vertexB l, nextACW_B l)
      | nextACW_B l == Just prevLath = (vertexA l, nextCW_A l)
      | nextCW_B l  == Just prevLath = (vertexA l, nextACW_A l)
      | otherwise = error "[SubZero] Improper mesh"

startScan::LathSet -> LathID -> LathSide -> (VertexID, Maybe LathID)
startScan ls lath side = Map.lookup lath ls >>= next
  where
    next l =
      if (vertexA l) > (vertexB l)
      then if side == RightSide
           then (vertexA l, nextACW_A l)
           else (vertexA l, nextCW_A l)
      else if side == RightSide
           then (vertexB l, nextCW_B l)
           else (vertexB l, nextACW_B l)


scanPolygon::LathSet -> LathID -> LathSide -> [VertexID]
scanPolygon ls start side = get fstGuess start
  where
    fstGuess = startScan ls start side
    get new last = case new of
      Just (vID, next)  ->
        if next == Just start
        then [vID]
        else case next of
          Just nL -> (vID):(get (nextOnFace ls nL last) nL)
          _       -> []
      Nothing -> []        


test_a = buildMesh [(1,6,7), (6,5,7), (5,4,7), (4,3,7), (3,2,7), (2,1,7)]
