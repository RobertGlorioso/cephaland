{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.HUD where

import Ceph.Components
import Ceph.Util

import Graphics.Gloss
import Linear
import Apecs

--needs functions that moves HUD objects when window resizes

debugToPic :: [Debug] -> [Picture]
debugToPic d = zipWith (\z (Debug str) -> Translate (-300) z $ Scale 0.07 0.07 $ color yellow $ Text str) [220,200..] d



hudPic :: System World [Picture]
hudPic = do
  [(Player, Position p, Velocity v, ProjCount numArrows, Health playerHP)] <- getAll  -- :: System World [(Player, ProjCount, Health)]
  
  Dash dashVal <- get global
  
  numEnemies <- return . length =<< (getAll :: System World [(Enemy,Position)])
  debugStrings <- getAll :: System World [Debug]
  debugPicture <- return $ debugToPic debugStrings 
  return $ [Translate 250 170 $ Scale 4 4 $ Line [((-8),10),(dashVal,10)],
            Color red $ Translate 250 150 $ ThickArc 0 (playerHP*pi) 2 59,
            Translate 230 220 $ Scale 0.1 0.1 $ Text $ show numArrows,
            Color green $ Translate 270 220 $ Scale 0.1 0.1 $ Text $ show numEnemies] ++ debugPicture 
