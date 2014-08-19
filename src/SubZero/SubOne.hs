{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Subdivision schema for open lines segments (1D). The open ends are assumed crease
-- vertices, and therefore are kept fixed during subdivision.
-- TODO: Generalize for open and closed lines with arbitrary number of crease vertices.
module SubZero.SubOne
       ( SubOneMesh
         ( subOnePointers
         , subOneLevel
         , subOneNSeg
         )
       , SubOne (..)
       , Level  (..)
       , NSegm  (..)
       , getSubOneArrSize
       , mkSubOne
       , mkSubOneFromMesh
       , mkSubOneMesh
       , subdivideOne
       , subdivideOneN
       , subOneTan
       , subOneLimit
       , renderSubOne
       ) where

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U

import           Data.Vector                 (Vector, (!))

import           Hammer.Math.Algebra
import           Hammer.VTK

-- =======================================================================================

newtype Level = Level Int deriving (Show, Eq, Num)
newtype NSegm = NSegm Int deriving (Show, Eq, Num)

-- | The @SubOne@ holds the mesh structure and the vertices values.
-- Assuming that all vertices of a mesh are control points at the level 0
-- of subdivision (no subdivision), they are fetched and stored in the
-- @subOnePoints@ array. On the subsequent subdivision new and updated vertex
-- are stored there as well.
data SubOne v =
  SubOne
  { subOneMesh   :: SubOneMesh  -- ^ Store the topological information.
  , subOnePoints :: Vector v    -- ^ Store the all the vertex positions.
  } deriving (Show)

-- | Holds the topological information about the subdivision line
data SubOneMesh =
  SubOneMesh
  { subOnePointers :: V.Vector Int -- ^ Indirect list of control points
  , subOneLevel    :: Level        -- ^ Current subdivision level
  , subOneNSeg     :: NSegm        -- ^ Initial number of segments
  } deriving (Show, Eq)

-- | Construct an one dimensional Subdivision for n segments at Level 0
mkSubOneMesh :: V.Vector Int -> Maybe SubOneMesh
mkSubOneMesh is
  | V.length is < 2 = Nothing
  | otherwise       = let
    ns = V.length is - 1
    in return $ SubOneMesh { subOnePointers = is
                           , subOneLevel    = Level 0
                           , subOneNSeg     = NSegm ns }

-- | Creates a subdivision line (1D) given a array points of control points and
-- pre-defined mesh structure for that array. Used to recalculate the mesh after updating
-- the control points. Warning: Giving the wrong control points array (size lower that the
-- highest control point reference in the mesh) will rise an error.
mkSubOneFromMesh :: V.Vector v -> SubOneMesh -> SubOne v
mkSubOneFromMesh vs mesh = let
  ps = V.map (vs V.!) (subOnePointers mesh)
  in SubOne mesh ps

-- | Construct an one dimensional Subdivision for n segments at Level 0
mkSubOne :: V.Vector Int -> V.Vector v -> Maybe (SubOne v)
mkSubOne is vs = mkSubOneMesh is >>= (return . mkSubOneFromMesh vs)

-- | Calculate the number of nodes in 'SubOne' for a given initial number of segments
-- after @l@ levels of subdivisions.
getSubOneArrSize :: NSegm -> Level -> Int
getSubOneArrSize (NSegm n) (Level l) = let
  each = (2 ^ l) + 1
  in n*each - (n-1)

subdivideOne :: (MultiVec v)=> SubOne v -> SubOne v
subdivideOne SubOne{..} = let
  levelUp  = let Level i = subOneLevel subOneMesh in Level (i+1)
  newSize  = getSubOneArrSize (subOneNSeg subOneMesh) levelUp
  newMax   = newSize - 1
  prevSize = V.length subOnePoints
  prevMax  = prevSize - 1
  wasNode  = even
  func i
    | i == 0      = subOnePoints!0
    | i == newMax = subOnePoints!prevMax
    | wasNode i   = (prevL &+ 6*&prev &+ prevH) &* (1/8)
    | otherwise   = (prev  &+ prevH) &* (1/2)
    where
      prevL  = subOnePoints!(previ-1)
      prev   = subOnePoints!previ
      prevH  = subOnePoints!(previ+1)
      previ  = i `div` 2
  newPoints = V.generate newSize func
  newMesh   = subOneMesh { subOneLevel = levelUp }
  in SubOne newMesh newPoints

subdivideOneN :: (MultiVec v)=> Int -> SubOne v -> SubOne v
subdivideOneN n sub
  | n <= 0    = sub
  | otherwise = subdivideOneN (n-1) (sub `seq` subdivideOne sub)

subOneLimit :: (MultiVec v)=> SubOne v -> Vector v
subOneLimit SubOne{..} = let
  nowmax = (V.length subOnePoints) - 1
  func i x
    | i == 0      = x
    | i == nowmax = x
    | otherwise   = (subOnePoints!(i-1) &+ (6 *& x) &+ subOnePoints!(i+1)) &* (1/8)
  in V.imap func subOnePoints

subOneTan :: (DotProd v, MultiVec v)=> SubOne v -> Vector v
subOneTan SubOne{..} = let
  nowmax = (V.length subOnePoints) - 1
  func i x
    | i == 0      = front &- x
    | i == nowmax = x     &- back
    | otherwise   = front &- back
    where
      front = subOnePoints!(i+1)
      back  = subOnePoints!(i-1)
  in V.imap (\i x -> normalize $ func i x) subOnePoints

-- =======================================================================================

-- | Render suddivision in VTK.
renderSubOne :: (U.Unbox v, RenderElemVTK v)=> SubOne v -> VTK v
renderSubOne sub = let
  subNArr = U.convert . subOnePoints $ sub
  line    = U.generate (U.length subNArr) id
  in mkUGVTK "SubOne" subNArr (V.singleton line) [] []

instance RenderCell (U.Vector Int) where
  makeCell   = id
  getType _  = VTK_POLY_LINE

instance RenderCell (Vector Int) where
  makeCell   = V.convert
  getType _  = VTK_POLY_LINE
