{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Ceph.Scene.HUD where

import Ceph.Scene.Board
import Ceph.Components
import Graphics.Gloss
import Linear (V2(..))
import Apecs


debugToPic :: ScreenBounds -> [Debug] -> [Picture]
debugToPic (SB (fmap fromIntegral ->  (V2 w h))) d = zipWith (\z (Debug str) -> Translate (-(w/2)) z $ Scale 0.07 0.07 $ color yellow $ Text str) [h/2 - 20,h/2 - 40..] d

beatCounter :: Beat -> ScreenBounds -> Picture
beatCounter (Beat m j) (SB (fmap fromIntegral -> (V2 w h))) =  Color blue $ Translate (w/2 - 40) (h/2 - 140) $ Circle (fromIntegral j)

hudPic :: Bool -> System World [Picture]
hudPic g = do
  --getDebugs
  screenbounds <- get global
  Entity 1 `set` In
  debugStrings <- cfoldM (\a b@(s,_) -> if s == In then  return (b:a) else return a ) [] :: System World [(Scope , Debug)]
  debugPicture <- if g then return $ debugToPic screenbounds (snd <$> debugStrings) else return []
  hud <- mkhud screenbounds
  
  return $ hud ++ debugPicture 
    where
      mkhud sb@(SB (fmap fromIntegral -> (V2 w h))) = do
        (sbd,sc) <- get global
        sbrd <- soundBoard sbd sc
        playerStats <- cfoldM (\a b -> return (b:a) ) []
        let (Player1, ProjCount numArrows, Health playerHP) = if length playerStats /= 0 then head playerStats else (Player1, ProjCount 0, Health 0)
        numThingsInScope <- length . filter (==In) <$> (cfoldM (\a b -> return (b:a) ) [] :: System World [(Scope)])
        Dash dashVal <- get global
        return $ [Translate (w/2 - 50) (h/2 - 100) $ Scale 4 4 $ Line [((-8),10),(dashVal,10)],
            Color red $ Translate (w/2 - 40) (h/2 - 70) $ ThickArc 0 (max 0 playerHP*pi) 2 59,
            Translate (w/2 - 50) (h/2 - 20) $ Scale 0.1 0.1 $ Text $ show numArrows,
            Color green $ Translate (w/2 - 20) (h/2 - 20) $ Scale 0.1 0.1 $ Text $ show numThingsInScope,
            Translate (w/2 - 40) (h/2 - 170) $ mempty ]
        
