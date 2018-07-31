{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.Camera where

import Ceph.Components

import Apecs
import Linear
import Graphics.Gloss

applyView :: Camera -> Picture -> Picture
applyView (Camera (V2 (realToFrac -> x) (realToFrac -> y)) (realToFrac -> scale)) = Scale scale scale . Translate (negate x) (negate y)

zoomIn :: Camera -> Camera
zoomIn (Camera c s) = (Camera c $ s+1)

zoomOut :: Camera -> Camera
zoomOut (Camera c s) = (Camera c $ s-1)

updateCamera :: Has World Camera => System World ()
updateCamera = do
  cmapM_ $ \(Player, Position p) -> modify global $ \(Camera o s) -> (Camera p s)
