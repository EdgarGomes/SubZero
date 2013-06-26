{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SubZero.SubOne
       ( SubOne
       , Level (..)
       , NSegm (..)
       , subOneArr
       , getSubOneArrSize
       , subOneLevel
       , subOneNSeg
       , mkSubOne
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
import           Hammer.Render.VTK.VTKRender

-- ==========================================================================================

newtype Level = Level Int deriving (Show, Eq, Num)
newtype NSegm = NSegm Int deriving (Show, Eq, Num)

data SubOne a = SubOne
  { subOneArr   :: V.Vector a
  , subOneLevel :: Level
  , subOneNSeg  :: NSegm
  } deriving (Show, Eq)

-- | Construct an one dimensional Subdivision for n segments at Level 0
mkSubOne :: V.Vector v -> Maybe (SubOne v)
mkSubOne arr
  |V.length arr >= 2 = let
    ns = V.length arr - 1
    in return $ SubOne { subOneArr   = arr
                       , subOneLevel = Level 0
                       , subOneNSeg  = NSegm ns }
  | otherwise        = Nothing

-- | Calculate the number of nodes in 'SubOne' for a given initial number of segments
-- after @l@ levels of subdivisions.
getSubOneArrSize :: NSegm -> Level -> Int
getSubOneArrSize (NSegm n) (Level l) = let
  each = (2 ^ l) + 1
  in n*each - (n-1)

subdivideOne :: (MultiVec v)=> SubOne v -> SubOne v
subdivideOne sub@SubOne{..} = let
  levelUp  = let Level i = subOneLevel in Level (i+1)
  newSize  = getSubOneArrSize subOneNSeg levelUp
  newMax   = newSize - 1
  prevSize = V.length subOneArr
  prevMax  = prevSize - 1
  wasNode  = even
  func i
    | i == 0      = subOneArr!0
    | i == newMax = subOneArr!prevMax
    | wasNode i   = (prevL &+ 6*&prev &+ prevH) &* (1/8)
    | otherwise   = (prev  &+ prevH) &* (1/2)
    where
      prevL  = subOneArr!(previ-1)
      prev   = subOneArr!previ
      prevH  = subOneArr!(previ+1)
      previ  = i `div` 2
  in sub {subOneArr = V.generate newSize func, subOneLevel = levelUp}

subdivideOneN :: (MultiVec v)=> Int -> SubOne v -> SubOne v
subdivideOneN n sub
  | n <= 0    = sub
  | otherwise = subdivideOneN (n-1) (sub `seq` subdivideOne sub)

subOneLimit :: (MultiVec v)=> SubOne v -> Vector v
subOneLimit SubOne{..} = let
  nowmax = (V.length subOneArr) - 1
  func i x
    | i == 0      = x
    | i == nowmax = x
    | otherwise   = (subOneArr!(i-1) &+ (6 *& x) &+ subOneArr!(i+1)) &* (1/8)
  in V.imap func subOneArr

subOneTan :: (DotProd v, MultiVec v)=> SubOne v -> Vector v
subOneTan SubOne{..} = let
  nowmax = (V.length subOneArr) - 1
  func i x
    | i == 0      = front &- x
    | i == nowmax = x     &- back
    | otherwise   = front &- back
    where
      front = subOneArr!(i+1)
      back  = subOneArr!(i-1)
  in V.imap (\i x -> normalize $ func i x) subOneArr



-- | Render suddivision in VTK.
renderSubOne :: Int -> SubOne Vec3 -> VTK Vec3
renderSubOne nmin sub = let
  (Level nl) = subOneLevel sub
  n = if nmin > nl then nmin - nl else 0
  subNArr :: U.Vector Vec3
  subNArr = U.convert $ subOneArr $ subdivideOneN n sub 
  line    = U.generate (U.length subNArr) id
  in mkUGVTK "SubOne" subNArr (V.singleton line)

instance RenderCell (U.Vector Int) where
  makeCell   = id
  getType _  = VTK_POLY_LINE

instance RenderCell (Vector Int) where
  makeCell   = V.convert
  getType _  = VTK_POLY_LINE