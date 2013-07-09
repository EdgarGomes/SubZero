{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SubZero
       ( mkSubOne
       , mkSubTwo
       , mkSubTwoFromMesh
       , SubZero       (..)
       , RenderSubZero (..)

         -- * SubOne (Lines)
       , subOneTan
       , subOneLimit
       , SubOne ( subOnePoints
                , subOneLevel
                , subOneNSeg
                )

         -- * SubTwo (Surfaces)
       , subTwoTans
       , subTwoLimit
       , subTwoNormals
       , SubTwo ( subTwoMesh
                , subTwoPoints
                )
       , buildMesh
       , MeshConn ( vertexType
                  , vertexConn
                  , edgeConn
                  , faceConn
                  , controlPointers
                  )
       , getSubTwoFaces
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
