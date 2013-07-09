-- | Module with some internal functions and data for advanced use. 
module SubZero.Internal
       ( -- * SubOne (Lines)
         Level (..)
       , NSegm (..)
       , getSubOneArrSize

         -- * SubTwo (Surfaces)
       , TableAccess (..)
       , VertexID    (..)
       , EdgeID      (..)
       , FaceID      (..)
       ) where

import SubZero.SubOne
import SubZero.SubTwo
