{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.Camera where

import Ceph.Components
import Ceph.Physics.Box

import Apecs
import Linear
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle        (radToDeg)

entsToPics :: (Scope, Position, Angle, BodyPicture) -> System World Picture
entsToPics (_, Position (V2 x y), Angle theta, BodyPicture pic) = return . Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic

applyView :: Camera -> Picture -> Picture
applyView (Camera (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> scale)) = Scale scale scale . Translate (negate x) (negate y)

zoomIn :: Camera -> Camera
zoomIn (Camera c s) = (Camera c $ s+1)

zoomOut :: Camera -> Camera
zoomOut (Camera c s) = (Camera c $ s-1)

cameraFollowPlayer :: (Player, Position) -> System World ()
cameraFollowPlayer (Player1, Position p) = modify global $ \(Camera o s) -> (Camera p s)
cameraFollowPlayer (_, Position p) = return () 

