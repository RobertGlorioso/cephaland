{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Ceph.Scene.HUD where

import Ceph.Components
import Ceph.Util
import Graphics.Gloss
import Linear
import Apecs

--needs functions that moves HUD objects when window resizes

debugToPic :: ScreenBounds -> [Debug] -> [Picture]
debugToPic (SB (fmap fromIntegral ->  (V2 w h))) d = zipWith (\z (Debug str) -> Translate (-(w/2)) z $ Scale 0.07 0.07 $ color yellow $ Text str) [h/2 - 20,h/2 - 40..] d

hudPic :: System World [Picture]
hudPic = do
  screenbounds <- get global  
  debugStrings <- getAll :: System World [Debug]
  debugPicture <- return $ debugToPic screenbounds debugStrings
  hud <- mkhud screenbounds 
  return $ hud ++ debugPicture 
    where
      mkhud (SB (fmap fromIntegral -> (V2 w h))) = do
        [(Player, Position p, Velocity v, ProjCount numArrows, Health playerHP)] <- getAll
        numEnemies <- return . length =<< (getAll :: System World [(Actor,Position)])
        Dash dashVal <- get global
        return $ [Translate (w/2 - 50) (h/2 - 100) $ Scale 4 4 $ Line [((-8),10),(dashVal,10)],
            Color red $ Translate (w/2 - 40) (h/2 - 70) $ ThickArc 0 (max 0 playerHP*pi) 2 59,
            Translate (w/2 - 50) (h/2 - 20) $ Scale 0.1 0.1 $ Text $ show numArrows,
            Color green $ Translate (w/2 - 20) (h/2 - 20) $ Scale 0.1 0.1 $ Text $ show numEnemies]
