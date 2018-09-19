{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.Camera where

import Ceph.Components
import Ceph.Physics.Box

import Apecs
import Linear
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle        (radToDeg)

entsToPics :: (Box, Position, Angle, BodyPicture) -> System World Picture
entsToPics (Box ((V2 x y), _ , _), _, Angle theta, BodyPicture pic) = return . Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic

applyView :: Camera -> Picture -> Picture
applyView (Camera (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> scale)) = Scale scale scale . Translate (negate x) (negate y)

zoomIn :: Camera -> Camera
zoomIn (Camera c s) = (Camera c $ s+1)

zoomOut :: Camera -> Camera
zoomOut (Camera c s) = (Camera c $ s-1)

updateCamera :: System World ()
updateCamera = do
  cmapM_ $ \( Player , Position p) -> modify global $ \(Camera o s) -> (Camera p s)

