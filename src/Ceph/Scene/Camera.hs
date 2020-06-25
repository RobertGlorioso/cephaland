{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.Camera where

import Ceph.Components
import Apecs
import Foreign.C.Types
import Linear

applyView :: Camera -> Position -> V2 CInt -> V2 CDouble
applyView (Camera cam scale) (Position p) size@(V2 x y) = 
  (p - cam + (fromIntegral <$> ( div <$> V2 (negate x) (negate y) <*> pure 2 )) ) / pure scale  + V2 390 260

unapplyView :: Camera -> Position -> V2 CInt -> V2 CDouble
unapplyView (Camera cam scale) (Position p) size@(V2 x y) = 
  (p + cam - (fromIntegral <$> ( div <$> V2 (negate x) (negate y) <*> pure 2 )) ) * pure scale  - V2 390 260

zoomIn :: Camera -> Camera
zoomIn (Camera c s) = (Camera c $ s+1)

zoomOut :: Camera -> Camera
zoomOut (Camera c s) = (Camera c $ s-1)

cameraFollowPlayer :: (Player, Position) -> System World ()
cameraFollowPlayer (Player1, Position p) = modify global $ \(Camera _ s) -> (Camera p s)
cameraFollowPlayer _ = return () 

