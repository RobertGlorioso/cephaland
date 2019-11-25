{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.Camera where

import Ceph.Components
import Apecs


zoomIn :: Camera -> Camera
zoomIn (Camera c s) = (Camera c $ s+1)

zoomOut :: Camera -> Camera
zoomOut (Camera c s) = (Camera c $ s-1)

cameraFollowPlayer :: (Player, Position) -> System World ()
cameraFollowPlayer (Player1, Position p) = modify global $ \(Camera _ s) -> (Camera p s)
cameraFollowPlayer _ = return () 

