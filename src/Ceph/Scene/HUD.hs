{-# LANGUAGE FlexibleContexts #-}
module Ceph.Scene.HUD where

import Ceph.Components

import Apecs
import Graphics.Gloss

debugPic :: [Debug] -> [Picture]
debugPic d = zipWith (\z (Debug str) -> Translate (-300) z $ Scale 0.07 0.07 $ color yellow $ Text str) [220,200..] d

hudPic :: System World [Picture]
hudPic = do
  [(Player, ProjCount i)] <- getAll :: System World [(Player, ProjCount)]
  Dash x <- get global
  ds <- getAll :: System World [Debug]
  d <- return $ debugPic ds 
  return $ [Translate 250 170 $ Scale 4 4 $ Line [((-8),10),(x,10)],
          Translate 250 220 $ Scale 0.1 0.1 $ Text $ show i] ++ d
