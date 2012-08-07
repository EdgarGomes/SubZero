{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SubZero.VertexEval where

import Prelude hiding (sum, zipWith, (++))
import Data.Maybe
import Data.Vector (Vector, (!), ifoldl', imap, sum, zipWith, (++))
import qualified Data.Vector as Vec

import Hammer.Math.Vector hiding (Vector)
import SubZero.Base
import SubZero.Subdivide (getNode)

normal patch ij = do
  t1 <- tanX patch ij
  t2 <- tanY patch ij
  return $ t1 &^ t2

tanY :: Patch Vec3 -> PatchPos -> Maybe Vec3
tanY patch ij = case isAtEdge patch ij of
  Nothing                     -> Nothing
  Just (Internal,      _, ns) -> internal ns
  Just (AtEdge Smooth, _, ns) -> external ns
  Just (AtEdge Corner, n, ns) -> corner n ns
  where
    corner n ns = let
      size = Vec.length ns
      in if size >= 2 then return $ Vec.head ns &- n else Nothing
    internal :: Vector Vec3 -> Maybe Vec3
    internal ns = let
      k = 2 * pi / (fromIntegral $ Vec.length ns)
      in return $ ifoldl' ( \acc i x -> acc &+ x &* cos(k * fromIntegral (i+1)) ) zero ns
    external :: Vector Vec3 -> Maybe Vec3
    external ns
      | size >= 2 = return $ Vec.head ns &- Vec.last ns
      | otherwise = Nothing
      where
        size = Vec.length ns

tanX :: Patch Vec3 -> PatchPos -> Maybe Vec3
tanX patch ij = case isAtEdge patch ij of
  Nothing                      -> Nothing
  Just (Internal,       _, ns) -> internal ns
  Just (AtEdge Smooth,  n, ns) -> external $ n `Vec.cons` ns
  Just (AtEdge Corner,  n, ns) -> corner n ns
  where
    corner n ns = let
      size = Vec.length ns
      in if size >= 2 then return $ Vec.last ns &- n else Nothing
    internal :: Vector Vec3 -> Maybe Vec3
    internal ns = let
      k = 2 * pi / (fromIntegral $ Vec.length ns)
      in return $ ifoldl' (\acc i x -> acc &+ x &* cos(k * fromIntegral i)) zero ns
    external :: Vector Vec3 -> Maybe Vec3
    external ns
      | size == 3 = applyMask $ Vec.fromList [-2, 1, 1]
      | size == 4 = applyMask $ Vec.fromList [-1, 0, 1, 0]
      | size == 5 = applyMask $ Vec.fromList [-2, -1, 2, 2, -1]
      | size >  5 = let
        z = pi / (fromIntegral $ size-1)
        func acc i x
          | i == 0    = acc
          | i == 1    = acc &+ (x &* (sin z))
          | i == size = acc &+ (x &* (sin z))
          | otherwise = acc &+ x &* ((2 * (cos z) - 2) * (sin (fromIntegral $ i-1) * z))
        in return $ ifoldl' func zero ns
      | otherwise     = Nothing
      where
        size = Vec.length ns
        applyMask = return . Vec.foldl' (&+) zero . zipWith (&*) ns

data VertexClass = Internal | AtEdge VertexType deriving (Show, Eq)

-- | Detects if the vertex is at edge or inside of the current patch and returns a list
-- with (current vertex, its neighbours in clockwise order).
isAtEdge::Patch Point3D -> PatchPos -> Maybe (VertexClass, Vec3, Vector Vec3)
isAtEdge patch@(TriPatch {..}) ij@(PatchPos (i,j))
  | i < 0 || i > n || j < 0 || j > i = Nothing
                                       
  | i == 0 && j == 0 = getNode patch ij >>= getLR v00Type mask3 nv00
  | i == n && j == n = getNode patch ij >>= getLR vnnType mask1 nvnn
  | i == n && j == 0 = getNode patch ij >>= getLR vn0Type mask2 nvn0
  
  | i == j           = atEdge ne00nn mask3
  | i == n           = atEdge nennn0 mask1
  | j == 0           = atEdge nen000 mask2

  | otherwise        = getNode patch ij >>= \x -> return (Internal, x, getFromMask mask2)
  where
    n = maxIx level    
    mask1 = Vec.fromList [(1,0),(0,-1),(-1,-1),(-1,0),(0,1),(1,1)]
    mask2 = Vec.fromList [(0,-1),(-1,-1),(-1,0),(0,1),(1,1),(1,0)]
    mask3 = Vec.fromList [(-1,-1),(-1,0),(0,1),(1,1),(1,0),(0,-1)]

    atEdge edge mask = getNode patch ij >>= \n ->
      if isJust edge
      then return (Internal, n,  getFromMask mask)
      else return (AtEdge Smooth, n, getFromMask mask)

    cleanMaybe = Vec.map (\(Just x) -> x) . Vec.filter isJust
    getFromMask = cleanMaybe . Vec.map (getNode patch . (+#+) ij . PatchPos)
    
    getLR::VertexType -> Vector (Int, Int) -> VertexConn (Vector Point3D) -> Vec3 -> Maybe (VertexClass, Vec3, Vector Vec3)
    getLR vt mask v n = return $ case v of
      OnRight x    -> (AtEdge vt, n, x ++ getFromMask mask)
      OnLeft  x    -> (AtEdge vt, n, getFromMask mask ++ x)
      BothLR x1 x2 -> (AtEdge vt, n, x2 ++ getFromMask mask ++ x1)
      JustOne x    -> (Internal , n, x ++ getFromMask mask)
      None         -> (AtEdge vt, n, getFromMask mask) 