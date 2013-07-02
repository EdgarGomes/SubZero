{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.Vector as V

import           Data.Vector        (Vector)
import           System.Environment (getArgs)

import           Hammer.Math.Algebra
import           Hammer.Render.VTK.VTKRender

import           SubZero

main :: IO ()
main = let
  path = "test"

  testOne = mkSubTwo ps [(3,5,2)] [2,3,4]
  testTwo = mkSubTwo ps [(3,4,2),(4,5,2),(5,6,2),(6,7,2),(7,3,2)] [3, 5, 7]
  testTwoInv = mkSubTwo ps [(6,5,2),(4,3,2),(2,4,5),(6,7,2),(3,2,7)] [3, 5, 7]
  ps = V.fromList [ Vec3 0 0 0, Vec3 0 0 0, Vec3 1 1 1, Vec3 0 0 0.5, Vec3 0 1 0
                  , Vec3 1 2 0, Vec3 (-2) 1 3, Vec3 (-1) 0 3, Vec3 0 0 0]

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
      name = path ++ "-SubTwo-" ++ show n ++ ".vtu"
      sz = subdivideN n testTwoInv

    writeUniVTKfile name (addNorm sz $ renderSub $ sz)