-----------------------------------------------------------------------------
--
-- Module      :  DeUniChecker
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Control.Applicative
import Control.Monad

import Data.Vector (Vector, (!), ifoldl')
import qualified Data.Vector as Vec

import Hammer.Math.Vector
import Hammer.Render.VTK.VTKRender

import SubZero.Base
import SubZero.Mesh
import SubZero.Subdivide



main =  do
  let myArgs = Args {replay = Nothing, maxSuccess = 1000, maxDiscard = 5000, maxSize = 1000, chatty = True}
  print "Testing Index / Array Position (Long Integer)"    
  quickCheckWith (myArgs {maxSuccess = 100000, maxDiscard = 500000}) prop_sn
  
  print "Testing Index / Array Position"    
  quickCheckWith myArgs prop_pos
  
error_precisson = (10e-3)

msgFail text = printTestCase ("\x1b[7m Fail: " ++ show text ++ "! \x1b[0m")


instance Arbitrary PatchPos where
  arbitrary = do
      p1 <- choose (0, 2^30)
      p2 <- choose (0, p1  )
      return $ PatchPos (p1, p2)

prop_sn::Integer -> Property
prop_sn i = (i > 0) ==> fulltest
  where
    fulltest = msgFail "Not reversible Sn(n) <--> n(Sn) " $ i == (invSn.sn) i

prop_pos::PatchPos -> Property
prop_pos ij = fulltest
  where
    p        = ix ij
    fulltest = msgFail "Not reversible ix <--> pos" $ ij == pos p



renderTest out n = writeMultiVTKfile (text2Path out) (renderTest patchs')
  where
    patchs'   = foldl (\acc x -> x acc) patchs (take n $ repeat subdAll)
    
    ts        = [(1,2,0),(2,3,0),(3,4,0),(4,5,0),(5,1,0)]
    mesh      = buildMesh ts
    positions = Vec.fromList [Vec3 1 1 1, Vec3 0 0 0, Vec3 0 2 0, Vec3 1 2 0, Vec3 2 1 0, Vec3 1 0 0]
    patchs    = Vec.map (evalPatch positions . makepatch mesh) (Vec.fromList ts)
    
    renderTest = Vec.map renderPatch 
    subdAll    = Vec.map subdivide

    testMesh = buildMesh [(1,5,2),(5,6,2),(5,8,6),(6,8,11),(6,11,13),(3,13,11)
                         ,(13,3,12),(10,13,12),(7,10,12),(12,4,7),(4,12,9)
                         ,(2,10,7),(2,7,1),(7,15,14),(1,7,14),(7,4,15)]