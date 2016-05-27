{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.Vector as V

import           Linear.Vect
import           Hammer.VTK

import           SubZero

main :: IO ()
main = do
  test2D "./" 0
  test2D "./" 3

  test1D "./" 0
  test1D "./" 3

vertices :: V.Vector Vec3
vertices = V.fromList [ Vec3 0 0 0, Vec3 0 0 0, Vec3 1 1 1, Vec3 0 0 0.5, Vec3 0 1 0
                      , Vec3 1 2 0, Vec3 (-2) 1 3, Vec3 (-1) 0 3, Vec3 0 0 0]

test1D :: FilePath -> Int -> IO ()
test1D path n = let
  addNorm sz x = let
    func i _ = (subOneTan sz) V.! i
    in addDataPoints x (mkPointAttr "norms" func)
  name = path ++ "SubOne-" ++ show n ++ ".vtu"
  in case mkSubOne (V.fromList [3, 4, 5]) vertices of
    Just ts -> let
      sz   = subdivideN n ts
      vtk  = addNorm sz $ renderSub $ sz
      in writeUniVTKfile name False vtk
    _ -> putStrLn "Can't make SubOne!"

test2D :: FilePath -> Int -> IO ()
test2D path n = let
  addNorm x = let
    func i _ = (subTwoNormals sz) V.! i
    in addDataPoints x (mkPointAttr "norms" func)
  ts = mkSubTwo vertices [(6,5,2),(4,3,2),(2,4,5),(6,7,2),(3,2,7)] [3, 5, 7]
  name = path ++ "SubTwo-" ++ show n ++ ".vtu"
  sz   = subdivideN n ts
  vtk  = addNorm $ renderSub $ sz
  in writeUniVTKfile name False vtk
