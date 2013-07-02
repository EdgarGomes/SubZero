{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SubZero
       ( mkSubOne
       , mkSubTwo
       , SubZero (..)
       , RenderSubZero (..)

         -- * SubOne (Lines)
       , subOneTan
       , subOneLimit
       , SubOne ( subOneArr
                , subOneLevel
                , subOneNSeg
                )
       , Level (..)
       , NSegm (..)
       , getSubOneArrSize

         -- * SubTwo (Surfaces)
       , SubTwo (..)
       , subTwoTans
       , subTwoLimit
       , subTwoNormals
       , buildMesh
       , MeshConn ( vertexType
                  , vertexConn
                  , edgeConn
                  , faceConn
                  , controlPointers
                  )
       , meshFaces
       , TableAccess (..)
       , VertexID (..)
       , EdgeID   (..)
       , FaceID   (..)
       ) where

import SubZero.SubOne
import SubZero.SubTwo

import Hammer.Math.Algebra
import Hammer.Render.VTK.VTKRender

class SubZero s where
  subdivide  :: s -> s
  subdivideN :: Int -> s -> s

class RenderSubZero s where
  renderSub :: s -> VTK Vec3


instance SubSuper Vec2
instance SubSuper Vec3

instance SubZero (SubOne Vec2) where
  subdivide  = subdivideOne
  subdivideN = subdivideOneN

instance SubZero (SubOne Vec3) where
  subdivide  = subdivideOne
  subdivideN = subdivideOneN

instance RenderSubZero (SubOne Vec3) where
  renderSub = renderSubOne


instance SubZero (SubTwo Vec2) where
  subdivide  = subdivideTwo
  subdivideN = subdivideTwoN

instance SubZero (SubTwo Vec3) where
  subdivide  = subdivideTwo
  subdivideN = subdivideTwoN

instance RenderSubZero (SubTwo Vec3) where
  renderSub = renderSubTwo
