{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SubZero.SubTwo
       ( SubTwo (..)
       , mkSubTwo
       , mkSubTwo'
       , subdivideTwo
       , subdivideTwoN
       , renderSubTwo
       , subTwoNormals 
       , subTwoTans
       , subTwoLimit 

       , buildMesh
       , MeshConn
       , meshFaces
       ) where

import           Data.Function

import           Control.Monad.Primitive (PrimState, PrimMonad)
import           Control.Monad.ST        (runST)
import           Data.IntMap             (IntMap)
import           Data.IntSet             (IntSet)
import           Data.Map                (Map)
import           Data.Vector             (Vector)
import           Data.Maybe              (isJust, mapMaybe)
  
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as VM
import qualified Data.IntMap         as IM
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.IntSet         as IS

import Hammer.Math.Algebra
import Hammer.Math.SortSeq
import Hammer.Render.VTK.VTKRender

--import           Debug.Trace
--dbg a = trace (">> " ++ show a) a
                  
-- ==============================================================================

newtype VertexID = VertexID
  { unVertexID :: Int
  } deriving (Show, Ord, Eq, Num)

newtype EdgeID = EdgeID
  { unEdgeID :: Int
  } deriving (Show, Eq, Ord, Num)

newtype FaceID = FaceID
  { unFaceID :: Int
  } deriving (Show, Eq, Ord, Num)

data VertexType
  = SmoothVertex
  | CornerVertex
    deriving (Show, Eq)

type VertexConn = (Vector EdgeID)
type EdgeConn   = (Maybe FaceID, Maybe FaceID, VertexID, VertexID)
type FaceConn   = (VertexID, VertexID, VertexID)

-- | Data structure for subdivision surface based on:
-- "A Mesh Data Structure for Rendering and Subdivision"
-- but with the edges also pointing back to their vertexs.
-- This data structure holds only connection information about
-- the mesh and the @controlPointers@ that point to an external
-- array containing the control point values that will be fetched before
-- the first subdivision step.
data MeshConn =
  MeshConn
  { vertexType      :: Vector VertexType -- ^ Creaseness of vertex
  , vertexConn      :: Vector VertexConn -- ^ Connecting edges for each vertex
  , edgeConn        :: Vector EdgeConn   -- ^ Faces and ending vertex for each edge
  , faceConn        :: Vector FaceConn   -- ^ Vertex of each face (triangle)
  , controlPointers :: Vector Int        -- ^ List of control points *pointers*
  } deriving (Show) 

-- | The @SubTwo@ holds the mesh structure and the vertices values.
-- Assuming that all vertices of a mesh are control points at the level 0
-- of subdivision (no subdivision), they are fetched and stored in the
-- @meshPoints@ array. On the subsequent subdivision new and updated vertex
-- are stored there as well.
data SubTwo =
  SubTwo
  { meshConn      :: MeshConn
  , meshPoints    :: Vector Vec3       -- ^ Store the all the vertex during subdivision.
  } deriving (Show)

class TableAccess a where
  type Ix a :: *
  readM   :: PrimMonad m => VM.MVector (PrimState m) a -> Ix a -> m a
  writeM  :: PrimMonad m => VM.MVector (PrimState m) a -> Ix a -> a -> m ()
  (=!)    :: Vector a -> Ix a -> a
  nullValue :: a

instance TableAccess VertexConn where
  type Ix VertexConn = VertexID
  readM  mv = VM.read  mv . unVertexID
  writeM mv = VM.write mv . unVertexID
  mv =! v   = mv V.! (unVertexID v)
  nullValue = V.empty

instance TableAccess EdgeConn where
  type Ix EdgeConn = EdgeID
  readM  mv = VM.read  mv . unEdgeID
  writeM mv = VM.write mv . unEdgeID
  mv =! v   = mv V.! (unEdgeID v)
  nullValue = (Nothing, Nothing, VertexID (-1), VertexID (-1))

instance TableAccess FaceConn where
  type Ix FaceConn = FaceID
  readM  mv = VM.read  mv . unFaceID 
  writeM mv = VM.write mv . unFaceID 
  mv =! v   = mv V.! (unFaceID v)
  nullValue = (VertexID (-1), VertexID (-1), VertexID (-1))

instance TableAccess Vec3 where
  type Ix Vec3           = VertexID
  readM  mv = VM.read  mv . unVertexID
  writeM mv = VM.write mv . unVertexID
  mv =! v   = mv V.! (unVertexID v)
  nullValue = zero

instance TableAccess VertexType where
  type Ix VertexType     = VertexID
  readM  mv = VM.read  mv . unVertexID
  writeM mv = VM.write mv . unVertexID
  mv =! v   = mv V.! (unVertexID v)
  nullValue = SmoothVertex

-- ==============================================================================

-- | Function to extract array of faces from a @MeshConn@
meshFaces :: MeshConn -> Vector (Int, Int, Int)
meshFaces = let
  foo (a, b, c) = (unVertexID a, unVertexID b, unVertexID c)
  in V.map foo . faceConn

-- | Creates a 2D mesh given a array points of control points
-- and pre-defined mesh structure for that array. Used to recalculate
-- the mesh after updating the control points.
-- Warning: Giving the wrong control points array (size lower that
-- the highest control point reference in the mesh) will rise an error.
mkSubTwo :: Vector Vec3 -> MeshConn -> SubTwo
mkSubTwo ps ms = SubTwo {
    meshConn   = ms
  , meshPoints = V.map (ps V.!) (controlPointers ms)
  }
    
-- | Creates a 2D mesh given a array points, a list of triangle that refers
-- to the array of points and a list of points that should be marked as
-- corners (their values are kept constant during the subdivision).
mkSubTwo' :: Vector Vec3 -> [(Int, Int, Int)] -> [Int] -> SubTwo
mkSubTwo' ps ts cs = mkSubTwo ps (buildMesh ts cs)

-- | Subdivide n times a mesh
subdivideTwoN :: Int -> SubTwo -> SubTwo
subdivideTwoN n sub
  | n <= 0    = sub
  | otherwise = subdivideTwoN (n-1) (sub `seq` subdivideTwo sub)

-- | Subdivide a mesh
subdivideTwo :: SubTwo -> SubTwo
subdivideTwo sb@SubTwo{..} = let
  subMesh = subdivideTwoConn   meshConn
  subArr  = subdivideTwoPoints meshConn meshPoints
  in sb { meshConn = subMesh, meshPoints = subArr }

-- | Subdivides the mesh connections, but keeps the previous vertex values
-- (meshPoints). This function returns a non-valid @MeshConn@ since the new
-- vertex aren't created yet and the old vertex aren't updated. Therefore use
-- the function @subdivideTwoPoints@ in order to calculate the new vertex array. 
subdivideTwoConn :: MeshConn -> MeshConn
subdivideTwoConn ms@MeshConn{..} = let
  vsize = V.length vertexConn
  esize = V.length edgeConn
  fsize = V.length faceConn
  
  -- Each face gives rise to 4 new faces
  newfsize = 4 * fsize
  -- Each edge gives rise to 1 new vertex
  newvsize = vsize + esize
  -- Lower part (2 x  num. of edges) is reserved for edge splitting.
  -- Each edge of a face (triangle) is split in two edge by the creation of
  -- a new vertex.
  -- 
  -- Upper part reserved for internal new edges. Each face will give
  -- rise to 4 new faces by adding a new internal face. This is done
  -- by connecting each new vertex pairwise and, therefore adding 3
  -- new internal edges.
  newesize = 2 * esize + 3 * fsize

  addvsize = esize
  eoffset  = 2 * esize
  
  f = V.replicate newfsize nullValue
  e = V.replicate newesize nullValue
  v = V.replicate newvsize nullValue
  t = vertexType V.++ V.replicate addvsize nullValue
  
  addF :: (PrimMonad m)
       => VM.MVector (PrimState m) FaceConn
       -> VM.MVector (PrimState m) EdgeConn
       -> VM.MVector (PrimState m) VertexConn
       -> FaceID
       -> (VertexID, VertexID, VertexID)
       -> m ()
  addF mf me mv fid face@(v1, v2, v3) = let
    -- The sort face's index were remove. It seems unnecessary and
    -- caused normal flipping (CW <-> CCW) on the triangular faces.
    errMsg = error $ "[SubTwo] I can't find edge of this face. It should be here." ++ show face
    (e12, e23, e31) = maybe errMsg id (findEdge face ms)

    -- Existent vertexs are updated and a new vertex is created for
    -- each new edge. Therefore the new vertex are added after them
    -- in the array. The new pos is uniquely defined as described
    -- bellow (size of the old vertex array + the position of the
    -- edge that creates the new vertex) 
    v12 = VertexID $ vsize + unEdgeID e12
    v23 = VertexID $ vsize + unEdgeID e23
    v31 = VertexID $ vsize + unEdgeID e31

    -- Each face creates 4 new faces that are placed in blocks
    -- Where:
    --   f1 - is the face connected to v1
    --   f2 - is the face connected to v2
    --   f3 - is the face connected to v3
    --   f4 - is the face in center
    f_1 = 4 * fid
    f_2 = 1 + f_1
    f_3 = 2 + f_1
    f_4 = 3 + f_1
  
    -- Each face creates 3 new internal edges that are placed in
    -- blocks of 3 at the upper part of the array (after eoffset).  
    newe1 = EdgeID $ eoffset + 3 * (unFaceID fid)
    newe2 = newe1 + 1
    newe3 = newe2 + 1
    
    in do
      -- keep clockwise convention v1 -> v2 -> v3
      writeM mf f_1 (v1,  v12, v31) 
      writeM mf f_2 (v2,  v23, v12) 
      writeM mf f_3 (v3,  v31, v23) 
      writeM mf f_4 (v12, v23, v31)

      writeM me newe1 (Just $ f_1, Just $ f_4, v31, v12)
      writeM me newe2 (Just $ f_2, Just $ f_4, v12, v23)
      writeM me newe3 (Just $ f_3, Just $ f_4, v23, v31)

      es12 <- readM mv v12
      writeM mv v12 (es12 V.++ (V.fromList [newe1, newe2]))

      es23 <- readM mv v23
      writeM mv v23 (es23 V.++ (V.fromList [newe2, newe3]))

      es31 <- readM mv v31
      writeM mv v31 (es31 V.++ (V.fromList [newe3, newe1]))


  addE :: (PrimMonad m)
       => VM.MVector (PrimState m) EdgeConn
       -> VM.MVector (PrimState m) VertexConn
       -> EdgeID
       -> EdgeConn
       -> m ()
  addE me mv eid (fida, fidb, vidA, vidB) = let
    -- Position of the new vertex created for each edge
    vidAB = VertexID $ vsize + unEdgeID eid

    -- Position of the 2 new split edges in blocks of 2 at
    -- the lower part of the array.
    eid1 = 2 * eid
    eid2 = eid1 + 1

    getNewFace Nothing    _   = Nothing
    getNewFace (Just fid) vid = let
      oldf@(v1, v2, v3) = faceConn =! fid
      fBlockOffset
        | vid == v3 = 2  -- for f3, f4 doesn't have conn. with splitting edges
        | vid == v2 = 1  -- for f2
        | vid == v1 = 0  -- for f1
        | otherwise = error $ "[SubTwo] I've found the wrong face!" ++ show oldf
      in return $ 4 * fid + fBlockOffset

    in do
      writeM me eid1 (getNewFace fida vidA, getNewFace fidb vidA, vidA, vidAB)
      writeM me eid2 (getNewFace fida vidB, getNewFace fidb vidB, vidAB, vidB)

      es1 <- readM mv vidA
      writeM mv vidA (es1 `V.snoc` eid1)
      es2 <- readM mv vidB
      writeM mv vidB (es2 `V.snoc` eid2)
      
      es12 <- readM mv vidAB
      writeM mv vidAB (es12 V.++ (V.fromList [eid1, eid2]))

  in runST $ do
    mf <- V.unsafeThaw f
    me <- V.unsafeThaw e
    mv <- V.unsafeThaw v
    V.zipWithM_ (addF mf me mv) (V.enumFromN (FaceID 0) fsize) faceConn
    V.zipWithM_ (addE me mv)    (V.enumFromN (EdgeID 0) esize) edgeConn
    ff <- V.unsafeFreeze mf
    fe <- V.unsafeFreeze me
    fv <- V.unsafeFreeze mv
    return $ ms { faceConn   = ff
                , edgeConn   = fe
                , vertexConn = fv
                , vertexType = t }

findEdge :: (VertexID, VertexID, VertexID) -> MeshConn
         -> Maybe (EdgeID, EdgeID, EdgeID)
findEdge (v1, v2 ,v3) MeshConn{..} = let
  es1 = vertexConn  =! v1
  es2 = vertexConn  =! v2
  es3 = vertexConn  =! v3
  foo vx vy = V.find (test vx vy)
  test vx vy i = let
    (_,_,va,vb) = edgeConn =! i
    in (va == vx && vb == vy) || (va == vy && vb == vx)
  in do
    e12 <- foo v1 v2 es1
    e23 <- foo v2 v3 es2
    e31 <- foo v3 v1 es3
    return (e12, e23, e31)

-- ==============================================================================

newtype Edge   = Edge (Int, Int) deriving (Show, Eq, Ord)
type FaceInfo  = Either Int (Int, Int)
type EdgeSet   = Map Edge (Int, FaceInfo)
type VertexSet = IntMap IntSet

-- | Create a Patch mesh  given a list of triangles and a list of quadruple
-- junctions. Provide all the triangles with the same clockwise sequence. 
-- OBS: - It doesn't work for mesh with holes
--      - It doesn't consider disjoints by one vertex (e.g. |><|)
buildMesh :: [(Int, Int, Int)] -> [Int] -> MeshConn
buildMesh ts corners = let
  (cp, inv_cp) = buildControlMaps ts
  ts' = mapMaybe (\(a,b,c) -> do
                a' <- IM.lookup a inv_cp
                b' <- IM.lookup b inv_cp
                c' <- IM.lookup c inv_cp
                return (a', b', c')) ts
  corners' = mapMaybe (\x -> IM.lookup x inv_cp) corners
  (vs, es) = buildMeshInfo ts'
  mesh = makepatch (vs, es, cp) ts' corners'
  in mesh

-- | Created a map from a linear seq of vertex ID [0,1,..] to
-- the real pointier array [2,5,50,..]. It also provides the inverse map.
buildControlMaps :: [(Int, Int, Int)] -> (Vector Int, IntMap Int)
buildControlMaps ts = let
  func acc (a,b,c) = IS.insert a $ IS.insert b $ IS.insert c acc
  vs    = IS.toList $ L.foldl' func IS.empty ts
  f_inv = IM.fromList $ zip vs [0..]
  f     = V.fromList vs
  in (f, f_inv)

-- | Allows ordering of EdgeID, such (a,b) = (b,a)                 
mkEdgeConn :: (Int, Int) -> Edge
mkEdgeConn (a, b)
  | a > b     = Edge (b, a)
  | otherwise = Edge (a, b)

-- | Create a set of connections for vertex and edges.
--        i            v       
--       /             |
-- t -- v -- j    j <- | -> t
--       \             |
--        k            i
buildMeshInfo :: [(Int, Int, Int)] -> (VertexSet, EdgeSet)
buildMeshInfo fs = L.foldl' addTriangle initfs (zip fs [0..])
  where initfs = (IM.empty, M.empty)

-- | Add a trianglulation from the mesh.
addTriangle :: (VertexSet, EdgeSet) -> ((Int, Int, Int), Int) -> (VertexSet, EdgeSet)
addTriangle (vs, es) ((a,b,c), fid) = (vAdder vs, es''')
  where
    vAdder = addVertex a eidAB eidCA .
             addVertex b eidAB eidBC .
             addVertex c eidBC eidCA
             
    (eidAB, es''') = addEdge a b es''
    (eidBC, es'')  = addEdge b c es'
    (eidCA, es')   = addEdge c a es

    addEdge x y ess = let
      e = mkEdgeConn (x, y)
      n = M.size ess 
      in case M.lookup e ess of
        Just (eid, info) -> (eid, M.insert e (eid, solveCollision info) ess)
        _                -> (n  , M.insert e (n, Left fid) ess)
    
    addVertex v e1 e2 vss
      | IM.member v vss = IM.adjust ins v vss
      | otherwise       = IM.insert v (IS.fromList [e1, e2]) vss
      where ins = IS.insert e1 . IS.insert e2
    
    -- A 2D surface edge can be assigned to max only 2 members (e.g. triangles) 
    solveCollision (Left e1)
      | e1 /= fid = Right (e1, fid)
      | otherwise = error "[SubTwo] Duplicated patch"
    solveCollision _ = error "[SubTwo] Invalide mesh!"

-- | Create a Patch (with its neighbour vertexs and edges) and
-- regarding the sequence of its neighbours according to the clockwise direction.
-- OBS: - It doesn't work for mesh with holes
--      - It doesn't consider disjoints by one vertex (e.g. |><|)
makepatch :: (VertexSet, EdgeSet, Vector Int) -> [(Int, Int, Int)]
          -> [Int] -> MeshConn
makepatch (vs, es, cp) fs creases
  | IM.null vs ||
    M.null es  ||
    null fs   = MeshConn nov nov nov nov nov 
  | otherwise = MeshConn
     { faceConn = V.map foo3 $ V.fromList fs
     , edgeConn = V.map foo1 . V.fromList . L.sortBy (compare `on` fst . snd) $
                  M.toList es
     , vertexConn = V.generate size foo2
     , vertexType = V.replicate size SmoothVertex V.//
                    (zip creases (repeat CornerVertex))
     , controlPointers = cp
     }
  where
    nov  = V.empty
    size = 1 + (fst $ IM.findMax vs)
    foo1 (Edge (e1, e2), (_, Left f1))        = ( Just $ FaceID f1
                                                , Nothing
                                                , VertexID e1
                                                , VertexID e2 )
    foo1 (Edge (e1, e2), (_, Right (f1, f2))) = ( Just $ FaceID f1
                                                , Just $ FaceID f2
                                                , VertexID e1
                                                , VertexID e2 )
    foo2 i = maybe V.empty (V.map EdgeID . V.fromList . IS.toList) (IM.lookup i vs)
    foo3 (a, b, c) = (VertexID a, VertexID b, VertexID c)


     
-- ==============================================================================
-- The masks and rules were based on the folliwng papers:
-- "Piecewise Smooth Surface Reconstruction"
-- "High Performance Subdivision Surfaces"
-- "Implementation of Triangle Subdivision for Holding Sharp Features with
-- Flatness Control"
-- ==============================================================================

subdivideTwoPoints :: MeshConn -> Vector Vec3 -> Vector Vec3
subdivideTwoPoints MeshConn{..} ps = let
  vsize = V.length vertexConn
  esize = V.length edgeConn
  updV :: (PrimMonad m)=> VM.MVector (PrimState m) Vec3 -> VertexID -> VertexConn -> m ()
  updV mv vid eids
    | isSmooth  = writeM mv vid v
    | otherwise = writeM mv vid (ps =! vid)
    where
      isSmooth = (vertexType =! vid) == nullValue
      es       = V.map (edgeConn =!) eids
      vtype    = vertexType =! vid
      v        = updateVertex ps vid vtype es
  newV :: (PrimMonad m) => VM.MVector (PrimState m) Vec3 -> EdgeID -> EdgeConn -> m ()
  newV mv eid e = let
    v   = newVertex ps faceConn e
    vid = VertexID $ vsize + unEdgeID eid
    in writeM mv vid v
  in runST $ do
    mv <- VM.new (vsize + esize)
    V.zipWithM_ (updV mv) (V.enumFromN (VertexID 0) vsize) vertexConn
    V.zipWithM_ (newV mv) (V.enumFromN (EdgeID   0) esize) edgeConn
    V.unsafeFreeze mv

updateVertex :: Vector Vec3 -> VertexID -> VertexType -> Vector EdgeConn -> Vec3
updateVertex ps vid vtype es
  | vtype == CornerVertex  = v
  | V.length onBoader == 2 = (1/8) *& ((6 *& v) &+ (sumAll onBoader))
  | V.length onBoader >= 2 = v
  | n                 >= 3 = ((1 - w) *& v) &+ ((w / dn) *& (sumAll es)) 
  | otherwise              = v -- Maybe an error should be rising.
  where
    n  = V.length es 
    dn = fromIntegral n
    w  = regularWeight n
    v  = ps =! vid
    sumAll = sumOtherEnd ps id vid
    (onBoader, _) = V.unstablePartition isOnBoader es
    
regularWeight :: Int -> Double 
regularWeight n = let
  dn = fromIntegral n
  a' = 3 + 2 * cos (2 * pi / dn)
  in (40 - a'*a') / 64

sumOtherEnd :: Vector Vec3 -> (Vec3 -> Vec3) -> VertexID -> Vector EdgeConn -> Vec3
sumOtherEnd ps func vid = let
  foo acc e = maybe acc (\i -> (func $ ps =! i) &+ acc) (getOtherEnd vid e) 
  in V.foldl' foo zero

getOtherEnd :: VertexID -> EdgeConn -> Maybe VertexID
getOtherEnd vid (_, _, v1, v2)
  | vid == v1 = return v2
  | vid == v2 = return v1
  | otherwise = Nothing
     
getOtherEnds :: VertexID -> Vector EdgeConn -> Vector VertexID
getOtherEnds vid = V.map (\(Just x) -> x) . V.filter isJust . V.map (getOtherEnd vid) 

isOnBoader :: EdgeConn -> Bool
isOnBoader (Just _, Just _, _, _) = False
isOnBoader _                      = True
                                    
withFace :: EdgeConn -> Bool
withFace (Just _, _, _, _) = True
withFace (_, Just _, _, _) = True
withFace _                 = False

newVertex :: Vector Vec3 -> Vector FaceConn -> EdgeConn -> Vec3
newVertex ps fs (Just f1, Just f2, v1, v2) = let
  s = (ps =! v1) &+ (ps =! v2) &+ sumFace ps (fs =! f1) &+ sumFace ps (fs =! f2)
  in (1/8) *& s
newVertex ps _ (_, _, v1, v2) = 0.5 *& ((ps =! v1) &+ (ps =! v2)) 
     
sumFace :: Vector Vec3 -> (VertexID, VertexID, VertexID) -> Vec3
sumFace ps (v1, v2, v3) = (ps =! v1) &+ (ps =! v2) &+ (ps =! v3)

-- ==============================================================================

-- | Calculate the limiting mesh e.g. the limiting position of each vertex
-- if the subdivision would be applied infinitely.
subTwoLimit :: SubTwo -> SubTwo
subTwoLimit sb@SubTwo{..} = let
  vsize = V.length meshPoints
  vids  = V.enumFromN (VertexID 0) vsize
  es    = edgeConn   meshConn 
  vct   = vertexType meshConn
  foo vid = limitPos meshPoints vid (vct =! vid) es
  in sb { meshPoints = V.map foo vids }

limitPos :: Vector Vec3 -> VertexID -> VertexType -> Vector EdgeConn -> Vec3
limitPos ps vid vtype es
  | vtype == CornerVertex  = v
  | V.length onBoader == 2 = (1/6) *& ((4 *& v) &+ (sumAll onBoader))
  | V.length onBoader >= 2 = v
  | n                 >= 3 = o *& v  &+ (sumAll es) 
  | otherwise              = v -- Maybe an error should be rising.
  where
    n  = V.length es 
    dn = fromIntegral n
    w  = regularWeight n
    o  = (3 * dn) / (8 * w)
    v  = ps =! vid
    sumAll = sumOtherEnd ps id vid
    (onBoader, _) = V.unstablePartition isOnBoader es

-- | Calculate the normal of each vertex.
subTwoNormals :: SubTwo -> Vector Vec3
subTwoNormals = V.map (\(t1, t2) -> t1 &^ t2) . subTwoTans

-- | Calculate two tangents (linear independent) for each vertex.
subTwoTans :: SubTwo -> Vector (Vec3, Vec3)
subTwoTans SubTwo{..} = let
  vsize = V.length meshPoints
  vids  = V.enumFromN (VertexID 0) vsize
  ecs   = edgeConn   meshConn 
  vcs   = vertexConn meshConn
  vct   = vertexType meshConn
  foo vid = tans meshPoints vid (vct =! vid) ecs (vcs =! vid) 
  in V.map foo vids
  
tans :: Vector Vec3 -> VertexID -> VertexType
     -> Vector EdgeConn -> Vector EdgeID -> (Vec3, Vec3)
tans ps vid vtype es eids
  | V.length looseE > 0   = error "[SubTwo] I don't know how find a tan in a vertex with loose edges."
  | otherwise             = case splitOpenLoop segs of
    ([OpenSeq a b vec], [])
      | vtype == CornerVertex && V.length vec > 1 -> let
        vs = getVS vec
        p  = ps =! vid
        -- Corner vertex
        t1 = creaseAlongTan (V.head vs) p
        t2 = creaseAlongTan p (V.last vs)
        in (t1, t2)
      | a == b && V.length vec > 1 -> let
        vs = getVS vec
        -- Crease vertex
        t1 = creaseAlongTan (V.head vs) (V.last vs)
        t2 = creaseAcrossTan (ps =! vid) vs
        in (t1, t2)
    ([], [LoopSeq vec])     -> let vs = getVS vec in (tan1 vs, tan2 vs)
    _                       -> error "[SubTwo] Strage mesh topology. I can't calc its tangent values."
  where
    getVS = V.map (ps =!) . getOtherEnds vid . V.map (es V.!)
    segs  = getVecSegsIndirect es (V.map unEdgeID properE)
    (properE, looseE) = V.unstablePartition (withFace . (es =!)) eids

instance SeqInv EdgeConn

instance SeqSeg EdgeConn where
  type SeqUnit EdgeConn = Maybe FaceID
  seqHead (h,_,_,_) = h
  seqTail (_,t,_,_) = t

instance SeqComp (Maybe FaceID) where
  seqComp Nothing _ = False
  seqComp _ Nothing = False
  seqComp a b = a == b

tan1 :: Vector Vec3 -> Vec3
tan1 vec = let
  size = V.length vec
  k    = 2 * pi / fromIntegral size
  calc acc i x = x &* cos (k * (fromIntegral i)) &+ acc
  in normalize $ V.ifoldl' calc zero vec

tan2 :: Vector Vec3 -> Vec3
tan2 vec = let
  size = V.length vec
  k    = 2 * pi / fromIntegral size
  calc acc i x = x &* cos (k * (fromIntegral $ i + 1)) &+ acc
  in normalize $ V.ifoldl' calc zero vec

creaseAlongTan :: Vec3 -> Vec3 -> Vec3
creaseAlongTan a b = normalize $ a &- b

creaseAcrossTan :: Vec3 -> Vector Vec3 -> Vec3
creaseAcrossTan v vec
  | size == 3 = mask $ V.fromList [-2, 1, 1]
  | size == 4 = mask $ V.fromList [-1, 0, 1, 0]
  | size == 5 = mask $ V.fromList [-2, -1, 2, 2, -1]
  | size >  5 = normalize $ V.ifoldl' func zero vec'
  | otherwise = error "[SubTwo] creaseAcrossTan expect vector size larger than 1"
  where
    vec' = v `V.cons` vec
    mask = normalize . V.foldl' (&+) zero . V.zipWith (&*) vec'
    size = V.length vec'
    z    = pi / (fromIntegral $ size-1)
    func acc i x
      | i == 0    = acc
      | i == 1    = acc &+ (x &* (sin z))
      | i == size = acc &+ (x &* (sin z))
      | otherwise = acc &+ x &* ((2 * (cos z)-2) * (sin (fromIntegral $ i-1) * z))

-- ==============================================================================
                          
-- | Prepare a subdivision patch for rendering.
renderSubTwo :: SubTwo -> VTK Vec3
renderSubTwo SubTwo{..} = let
  ts = faceConn meshConn
  tr :: U.Vector Vec3
  tr = V.convert $ meshPoints 
  in mkUGVTK "SubTwo" tr ts

instance RenderCell FaceConn where
  makeCell (VertexID a, VertexID b, VertexID c) = makeCell (a, b, c)
  getType _                                     = VTK_TRIANGLE