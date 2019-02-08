{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Ceph.Scene.HUD where

import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box
import Graphics.Gloss
import Linear
import Apecs
import Data.List

--needs functions that moves HUD objects when window resizes

debugToPic :: ScreenBounds -> [Debug] -> [Picture]
debugToPic (SB (fmap fromIntegral ->  (V2 w h))) d = zipWith (\z (Debug str) -> Translate (-(w/2)) z $ Scale 0.07 0.07 $ color yellow $ Text str) [h/2 - 20,h/2 - 40..] d

hudPic :: Bool -> System World [Picture]
hudPic g = do
  screenbounds <- get global  
  debugStrings <- getAll :: System World [Debug]
  debugPicture <- if g then return $ debugToPic screenbounds debugStrings else return []
  hud <- mkhud screenbounds
  return $ hud ++ debugPicture 
    where
      mkhud (SB (fmap fromIntegral -> (V2 w h))) = do
        [(Player1, ProjCount numArrows, Health playerHP)] <- getAll
        numThingsInScope <- length . filter (==In) <$> (getAll :: System World [(Scope)])
        Dash dashVal <- get global
        return $ [Translate (w/2 - 50) (h/2 - 100) $ Scale 4 4 $ Line [((-8),10),(dashVal,10)],
            Color red $ Translate (w/2 - 40) (h/2 - 70) $ ThickArc 0 (max 0 playerHP*pi) 2 59,
            Translate (w/2 - 50) (h/2 - 20) $ Scale 0.1 0.1 $ Text $ show numArrows,
            Color green $ Translate (w/2 - 20) (h/2 - 20) $ Scale 0.1 0.1 $ Text $ show numThingsInScope]
