{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SubZero.Subdivide
       (subdivide)
where

import Control.Monad  
import Data.Maybe
import Data.Vector (Vector, (!), ifoldl')
import qualified Data.Vector as Vec

import Hammer.Math.Vector hiding (Vector)
import SubZero.Base
import SubZero.Mesh

subdivide::Patch Point3D -> Patch Point3D
subdivide patch@(TriPatch {..}) =
  patch { level     = level + 1
        , nv00      = new_nv00
        , ne00nn    = new_ne00nn
        , nvnn      = new_nvnn
        , nennn0    = new_nennn0
        , nvn0      = new_nvn0
        , nen000    = new_nen000
        , triMatrix = newpatch }
  where
    newSize    = triMatrixSize (level + 1)
    newN       = maxIx (1 + level)
    n          = maxIx level
    newpatch   = Vec.generate newSize func
    
    new_ne00nn = genNewEdge ne00nn (\i -> PatchPos (i       , i+1   ))
    new_nennn0 = genNewEdge nennn0 (\i -> PatchPos (newN+1  , newN-i))
    new_nen000 = genNewEdge nen000 (\i -> PatchPos (newN-i-1, -1    ))
                     
    new_nv00   = newVertex (getNodeTM (0,0)) nen000 nv00 ne00nn
    new_nvnn   = newVertex (getNodeTM (n,n)) ne00nn nvnn nennn0
    new_nvn0   = newVertex (getNodeTM (n,0)) nennn0 nvn0 nen000

    getNodeTM = (triMatrix!) . ix . PatchPos      
    
    genNewEdge old genPos =
      if isJust old
        then let gen = (newEdge patch) . genPos 
             in return $ Vec.generate newN gen
        else Nothing
    
    func i
      | isNode ij = updateNode patch ij
      | otherwise = newEdge patch ij
      where ij = pos i


newVertex::Point3D -> Maybe (Vector Point3D) -> VertexConn (Vector Point3D) -> Maybe (Vector Point3D) -> VertexConn (Vector Point3D)
newVertex vertex l v r = case v of
  OnRight x    -> OnRight $ calcEdge Nothing x r
  OnLeft  x    -> OnLeft  $ calcEdge l x Nothing
  BothLR x1 x2 -> BothLR (calcEdge l x1 Nothing) (calcEdge Nothing x2 r)
  JustOne x    -> JustOne $ calcEdge l x r
  None         -> None
  where
    calcEdge l vec r = let
      func i e = 
        let ea = getNode (i-1)
            eb = getNode (i+1)
            p = if isJust ea && isJust eb
                then liftM2 (calcNormEdge vertex e) ea eb
                else return $ calcCreaseEdge vertex e
        in checkNode p 
                  
      getNode i
        | i < 0               = fmap Vec.last l
        | i >= Vec.length vec = fmap Vec.head r
        | otherwise           = return $ vec ! i
      in Vec.imap func vec


calcNormEdge e1 e2 ea eb = (3 *& (e1 &+ e2) &+ ea &+ eb) &* (1/8)
    
calcCreaseEdge e1 e2 = 0.5 *& (e1 &+ e2)

newEdge::Patch Point3D -> PatchPos -> Point3D
newEdge patch ij
  | isEdge (PatchPos (1,0)) = calcNew (PatchPos (1,0)) (PatchPos (1,1)) (PatchPos (0,-1))
  | isEdge (PatchPos (1,1)) = calcNew (PatchPos (1,1)) (PatchPos (1,0)) (PatchPos (0, 1))
  | isEdge (PatchPos (0,1)) = calcNew (PatchPos (0,1)) (PatchPos (1,1)) (PatchPos (-1,0))
  where
    isEdge delta = isNode (ij +#+ delta) && isNode (ij -#- delta)

    calcNew edgeDir aDir bDir = let
      ij1 = prevNode (ij +#+ edgeDir)
      ij2 = prevNode (ij -#- edgeDir)
      ea  = getNodeExtra patch (ij2 +#+ aDir)
      eb  = getNodeExtra patch (ij2 +#+ bDir)
      e1  = getNodeExtra patch ij1
      e2  = getNodeExtra patch ij2
      p = if isJust ea && isJust eb
          then liftM4 calcNormEdge e1 e2 ea eb
          else liftM2 calcCreaseEdge e1 e2
      in checkNode p

updateNode::Patch Point3D -> PatchPos -> Point3D
updateNode patch@(TriPatch {..}) ij
  | i == 0 && j == 0 = if v00Type == Corner
                       then checkNode $ getNode patch oldij
                       else getVertex nen000 (PatchPos (1,0))     nv00 ne00nn (PatchPos (1,1))
  | i == n && j == n = if vnnType == Corner
                       then checkNode $ getNode patch oldij
                       else getVertex ne00nn (PatchPos (n-1,n-1)) nvnn nennn0 (PatchPos (n,n-1))
  | i == n && j == 0 = if vn0Type == Corner
                       then checkNode $ getNode patch oldij
                       else getVertex nennn0 (PatchPos (n,1))     nvn0 nen000 (PatchPos (n-1,0))
  
  | i == j           = if isJust ne00nn then calcNode getAll else calcCreaseNode (PatchPos (1,1))
  | i == n           = if isJust nennn0 then calcNode getAll else calcCreaseNode (PatchPos (0,1))
  | j == 0           = if isJust nen000 then calcNode getAll else calcCreaseNode (PatchPos (1,0))

  | otherwise        = calcNode getAll
  where
    n = maxIx level
    oldij@(PatchPos (i,j)) = prevNode ij
    
    mask = let ps = Vec.fromList [(1,0),(-1,0),(1,1),(-1,-1),(0,1),(0,-1)]
           in Vec.map ((+#+) oldij . PatchPos) ps
    
    getAll = Vec.foldl' consJust Vec.empty mask
    
    calcCreaseNode x = let
      a = checkNode $ getNode patch $ oldij +#+ x 
      b = checkNode $ getNode patch $ oldij -#- x
      in crease a b
    
    calcNode vec = let
      v    = checkNode $ getNode patch oldij
      n    = Vec.length vec 
      beta = if n > 3 then 3/(fromIntegral $ 8*n) else 3/16
      k    = 1 - beta * (fromIntegral n)
      func acc x = acc &+ (x &* beta)
      in k *& v &+ Vec.foldl' func zero vec
    
    consJust::Vector Point3D -> PatchPos -> Vector Point3D
    consJust xs x = case getNode patch x of
      Just n  -> Vec.cons n xs
      Nothing -> xs
    
    getVertex leftEdge leftPos v rightEdge rightPos = let
      left  = tryElse (fmap Vec.last leftEdge)  (getNode patch leftPos)
      right = tryElse (fmap Vec.head rightEdge) (getNode patch rightPos)
      tryElse a b = checkNode $ if isJust a then a else b 
      in calcVertex left v right
                       
    calcVertex::Point3D -> VertexConn (Vector Point3D) -> Point3D -> Point3D
    calcVertex left v right = case v of
      OnRight x    -> crease left (Vec.head x)
      OnLeft  x    -> crease (Vec.last x) right
      BothLR x1 x2 -> crease (Vec.last x1) (Vec.head x2)
      JustOne x    -> calcNode (getAll Vec.++ x)
      None         -> crease left right
      
    crease::Point3D -> Point3D -> Point3D
    crease l r = case getNode patch oldij of
      Just v -> (1/8) *& (l &+ r &+ 6 *& v)
      _      -> getErr ij


checkNode mn = case mn of
  Just n -> n
  _      -> error "[SubZero] Invalid patch!"
    

getErr ij = error ("[SubZero] The position " ++ show ij ++ " is out of bound!")


getNode::Patch a -> PatchPos -> Maybe a
getNode patch@(TriPatch {..}) ij@(PatchPos (i,j))
  | i >=  0  && i < n && j == i+1 = ne00nn !|! (i) 
  | i == n+1 && j > 0 && j <= n   = nennn0 !|! (n-j)
  | i >= 0   && i < n && j == -1  = nen000 !|! (n-i-1) 
  
  | i >= 0 && i <= n && 
    j >= 0 && j <= i          = return $ triMatrix!(ix ij)

  | otherwise                 = Nothing
  where
    n = maxIx level
    vec !|! ix = fmap (!ix) vec


getNodeExtra::Patch a -> PatchPos -> Maybe a
getNodeExtra patch@(TriPatch {..}) ij@(PatchPos (i,j))  
  | i == -1  && j == 0   = snd $ getLR nv00
  | i == n   && j == n+1 = fst $ getLR nvnn
  | i == n+1 && j == n+1 = snd $ getLR nvnn
  | i == n+1 && j == 0   = fst $ getLR nvn0
  | i == n   && j == -1  = snd $ getLR nvn0
  | i == -1  && j == -1  = fst $ getLR nv00
  | otherwise            = getNode patch ij
  where    
    n = maxIx level
    
    getLR::VertexConn (Vector a) -> (Maybe a, Maybe a)
    getLR v = case v of
      OnRight x    -> (Nothing,              return $ Vec.last x )
      OnLeft  x    -> (return $ Vec.head x,  Nothing             )
      BothLR x1 x2 -> (return $ Vec.head x1, return $ Vec.last x2)
      JustOne x    -> (return $ Vec.head x,  return $ Vec.last x )
      None         -> (Nothing,              Nothing             )   
    
    

