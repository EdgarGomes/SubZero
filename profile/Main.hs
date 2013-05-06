{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.Vector as V
  
import           Data.Vector        (Vector)
import           System.Environment (getArgs)

import           Hammer.Math.Algebra
import           Hammer.Render.VTK.VTKRender

import           SubZero.SubOne
import           SubZero.SubTwo

main :: IO ()
main = let
  path = "test"
  
  testOne = mkSubTwo (V.take 3 ps) [(1,2,0)] [0,1,2]
  testTwo = mkSubTwo ps [(1,2,0),(2,3,0),(3,4,0),(4,5,0),(5,1,0)] [1, 3, 5]
  ps = V.fromList [ Vec3 1 1 1, Vec3 0 0 0.5, Vec3 0 1 0
                  , Vec3 1 2 0, Vec3 (-2) 1 3, Vec3 (-1) 0 3]
  
  addNorm sz x = let
    func i _ = (subTwoNormals sz) V.! i
    in addDataPoints x (mkPointAttr "norms" func)

  in do
    args <- getArgs
    let
      n :: Int
      n = case args of
        [ns] -> read ns
        _    -> error "Insert the number of subdivision steps!"
    case testTwo of
      Just s -> let
        name = path ++ "-SubTwo-" ++ show n ++ ".vtu" 
        sz = subdivideTwoN n s 
        in writeUniVTKfile name (addNorm sz $ renderSubTwo $ sz) 
      _ -> print "Can't create mesh."