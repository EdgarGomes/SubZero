{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SubZero.Base
where

import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Maybe
import Data.Vector (Vector, (!), ifoldl')
import qualified Data.Vector as Vec

import Control.Monad

import Hammer.Math.Vector hiding (Vector)
import Hammer.Render.VTK.VTKRender
import Hammer.Render.VTK.Base

{--
import Debug.Trace

debug :: Show a => String -> a -> a
debug t x = trace (t ++ " " ++ show x ++ "\n") x
--}

type Point3D = Vec3
type PatchID = Int

newtype PatchPos = PatchPos (Int,Int)  deriving (Show, Eq)

data VertexConn a = OnRight a
                  | OnLeft a
                  | BothLR a a
                  | JustOne a
                  | None
                  deriving (Show, Eq)

instance Functor VertexConn where
  fmap f a = case a of
     OnRight x    -> OnRight (f x)
     OnLeft x     -> OnLeft  (f x)
     BothLR x1 x2 -> BothLR  (f x1) (f x2)
     JustOne x    -> JustOne (f x)
     None         -> None

data VertexType = Smooth
                | Corner
                deriving (Show, Eq)

-- | The Patch data defines the central triangular patch (a triangular matrix)
-- and the first neighbours for each vertex and edge.                      
-- 
--         (0, 0)
--           /\ 
--          /  \
--         /    \
--      (n,0)__(n,n)
data Patch a = TriPatch 
           { level     :: Int                   -- ^ Level of subdivision
           , nv00      :: VertexConn (Vector a) -- ^ Neighbors of vertex (0,0)
           , v00Type   :: VertexType 
           , ne00nn    :: Maybe (Vector a)      -- ^ Neighbors of edge (0,0) -- (n,n)
           , nvnn      :: VertexConn (Vector a) -- ^ Neighbors of vertex (n,n)
           , vnnType   :: VertexType 
           , nennn0    :: Maybe (Vector a)      -- ^ Neighbors of edge (n,n) -- (n,0)
           , nvn0      :: VertexConn (Vector a) -- ^ Neighbors of vertex (n,0)
           , vn0Type   :: VertexType 
           , nen000    :: Maybe (Vector a)      -- ^ Neighbors of edge (n,0) -- (0,0)
  
           , triMatrix ::Vec.Vector a          -- ^ Triangular matrix
           } deriving (Show, Eq)                     
                      

(+#+)::PatchPos -> PatchPos -> PatchPos
(PatchPos (i1,j1)) +#+ (PatchPos (i2,j2)) = (PatchPos (i1+i2, j1+j2))

(-#-)::PatchPos -> PatchPos -> PatchPos
(PatchPos (i1,j1)) -#- (PatchPos (i2,j2)) = (PatchPos (i1-i2, j1-j2))


-- Might be limited by Int (max 2^32 or 2^64). If it is the case, use Integer (no limit but slower).
-- Eventhough, 2^32 should be more than enough due its use as index.
sn::(Integral a)=> a -> a
sn n = n*(1+n) `div` 2

-- to calc the inverse of the Sn(n), i.e. n(Sn), with no recursion function (execept sqrt) 
-- it might be faster but involves float point operation, thus it must be deeply tested
invSn::(Integral a)=> a -> a
invSn sn = let delta = sqrt (1 + 8 * fromIntegral sn) in (floor $ (-1 + delta)/2)

ix::PatchPos -> Int
ix (PatchPos (i,j)) = sn i + j
                                   
pos::Int -> PatchPos
pos ix = let n = invSn ix in PatchPos (n, ix - sn n)

maxIx::Int -> Int
maxIx n = 2^n

triMatrixSize::Int -> Int
triMatrixSize = sn . (+1) . maxIx

posAlt::Int -> PatchPos
posAlt ix = let (i, total) = func 0 in PatchPos (i, ix - total)
  where
    func n
      | sn (n+1) > ix = (n, sn n)
      | otherwise     = let x = func (n+1) in x `seq` x


isNode (PatchPos (i,j)) = even i && even j
prevNode (PatchPos (i,j)) = let f x = x `div` 2 in PatchPos (f i, f j)


evalPatch::Vector Point3D -> Patch Int -> Patch Point3D
evalPatch table p =
  p { level     = level p
    , nv00      = fmap eval $ nv00 p
    , ne00nn    = fmap eval $ ne00nn p
    , nvnn      = fmap eval $ nvnn p
    , nennn0    = fmap eval $ nennn0 p
    , nvn0      = fmap eval $ nvn0 p
    , nen000    = fmap eval $ nen000 p
    , triMatrix = eval $ triMatrix p }
  where
    eval = Vec.map (table!)
    
-- | Extract all triangles from a subdivision patch.    
patchToTriangles::Patch a -> (Vector (Int,Int,Int), Vector a)
patchToTriangles patch@(TriPatch {..}) = let
  ts = ifoldl' func Vec.empty triMatrix
  isInPatch (PatchPos (i,j)) = let 
    n = maxIx level
    in i >= 0 && i <= n && j >= 0 && j <= i
  func acc i x = let
    ij  = pos i
    t1a = ij +#+ PatchPos (1,0)
    t1b = ij +#+ PatchPos (1,1)
    t2a = ij -#- PatchPos (1,0)
    t2b = ij -#- PatchPos (1,1)
    mkT a b = if isInPatch a && isInPatch b
         then Vec.singleton (ix a, i, ix b)
         else Vec.empty
    in acc Vec.++ mkT t1a t1b Vec.++ mkT t2a t2b
  in (ts, triMatrix)

-- | Prepare a subdivision patch for rendering.
renderPatch::Patch Vec3 -> VTK Point3D
renderPatch patch = let
  (ts, triMatrix) = patchToTriangles patch
  in mkUGVTK "patch" triMatrix ts