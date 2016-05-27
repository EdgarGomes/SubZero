{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SubZero
  ( mkSubOne
  , mkSubTwo
    -- * From mesh
  , mkSubTwoFromMesh
  , mkSubOneFromMesh
    -- * Build mesh
  , buildMesh
  , mkSubOneMesh
    -- * Common functions
  , SubZero       (..)
  , RenderSubZero (..)

    -- * SubOne (Lines)
  , subOneTan
  , subOneLimit
  , SubOne
    ( subOneMesh
    , subOnePoints
    )
  , SubOneMesh
    ( subOnePointers
    , subOneLevel
    , subOneNSeg
    )

    -- * SubTwo (Surfaces)
  , subTwoTans
  , subTwoLimit
  , subTwoNormals
  , SubTwo
    ( subTwoMesh
    , subTwoPoints
    )
  , MeshConn
    ( vertexType
    , vertexConn
    , edgeConn
    , faceConn
    , controlPointers
    )
  , getSubTwoFaces
  ) where

import Hammer.VTK
import Linear.Vect

import SubZero.SubOne
import SubZero.SubTwo

class SubZero s where
  subdivide  :: s -> s
  subdivideN :: Int -> s -> s

class RenderSubZero s where
  renderSub :: s -> VTK Vec3D


instance SubSuper Vec2D
instance SubSuper Vec3D

instance SubZero (SubOne Vec2D) where
  subdivide  = subdivideOne
  subdivideN = subdivideOneN

instance SubZero (SubOne Vec3D) where
  subdivide  = subdivideOne
  subdivideN = subdivideOneN

instance RenderSubZero (SubOne Vec3D) where
  renderSub = renderSubOne


instance SubZero (SubTwo Vec2D) where
  subdivide  = subdivideTwo
  subdivideN = subdivideTwoN

instance SubZero (SubTwo Vec3D) where
  subdivide  = subdivideTwo
  subdivideN = subdivideTwoN

instance RenderSubZero (SubTwo Vec3D) where
  renderSub = renderSubTwo
