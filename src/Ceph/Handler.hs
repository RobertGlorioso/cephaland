{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Handler where

import Ceph.Components
import Ceph.Util
import Ceph.Component.Projectile

import Apecs
import Apecs.Util
import Control.Monad
import Graphics.Gloss.Interface.IO.Game hiding ( Play )
import Euterpea
import qualified SDL.Mixer as M
import Linear

mouseToWorld :: (Float,Float) -> Camera -> V2 Float
mouseToWorld (x,y) (Camera offset scale) = offset + V2 x y 

handle :: Event -> System World ()
handle (EventResize (m1, m2)) = do
  modify global $ \(SB _) -> SB (V2 m1 m2)
handle (EventMotion mscreen) = do
  mpos <- mouseToWorld mscreen <$> get global
  cmap $ \(Target _) -> Target mpos
  
handle (EventKey press downup modifiers mscreen) = do
  case (press, downup) of
    (SpecialKey KeyRight, Down) -> do
      cmap $ movePlayer (V2 0.05 0)
    (Char 'd', Down) ->
      cmap $ movePlayer (V2 0.05 0)
    (SpecialKey KeyRight, Up) ->
      cmap $ movePlayer (V2 0.05 0)
    (Char 'd', Up) ->
      cmap $ movePlayer (V2 0.05 0)
    (SpecialKey KeyLeft, Down) ->
      cmap $ movePlayer (V2 (-0.05) 0)
    (Char 'a', Down) ->
      cmap $ movePlayer (V2 (-0.05) 0)
    (SpecialKey KeyLeft, Up) ->
      cmap $ movePlayer (V2 (-0.02) 0)
    (Char 'a', Up) ->
      cmap $ movePlayer (V2 (-0.02) 0)
    (SpecialKey KeyUp, Down) ->
      cmap $ movePlayer (V2 0 0.06) 
    (Char 'w', Down) ->
      cmap $ movePlayer (V2 0 0.06)
    (SpecialKey KeyUp, Up) ->
      cmap $ movePlayer (V2 0.0 0.03)
    (Char 'w', Up) ->
      cmap $ movePlayer (V2 0.0 0.03)
    (SpecialKey KeyDown, Down) ->
      cmap $ movePlayer (V2 0.0 (-0.02))
    (Char 's', Down) ->
      cmap $ movePlayer (V2 0.0 (-0.02))
    (MouseButton RightButton, Down) -> do
      cmapM_ $ \(Dash w) -> 
        if w >= 8.0
        then do
          cmapM_ $ \(Target o) -> cmap $ \(Player, Position p, _ :: Behavior) -> (NoBehavior, Velocity $ 0.5 * normalize (o - p))
          cmap $ \(Dash w) -> Dash (-8.0)
        else return ()
      cmap $ \(Player) -> (Player, Attacking)
                                        
    (MouseButton RightButton,Up) -> do
      cmapM_ $ \c@(Player,Attacking, e) -> destroy e (Proxy :: Proxy Attacking)
    
    (SpecialKey KeySpace, Up) -> do
      cmap $ \(Player, b :: Behavior) -> (Player, NoBehavior)
    (SpecialKey KeySpace, Down) ->
      cmap $ \(Player) -> (Player, Carry)
                          
    (e, f) -> return () -- -- liftIO (print e >> print f >> return ())
    
  case (press, downup, modifiers) of
    (MouseButton LeftButton,Down,Modifiers Down Up Up) -> cmapM_ removeProjectile
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> cmap $ \(Player, Charge c _) -> (Player, Charge c True)

    (MouseButton LeftButton,Up,Modifiers _ _ _) -> do
      cmapM_ $ \(c, x, v, ProjCount arrowsLeft, Player, e) -> do
        t <- get global 
        --cmap $ \(ProjCount n, Player) -> (Player, ProjCount $ arrowsLeft - 1)
        when (arrowsLeft >= 1) $ do
          --cmapM_ $ \(Player,Resources _ _ p) -> if p == [] then return () else M.play $ head p
          shootArrow t x v c
          e `set` (Charge 0.25 False, x, v, ProjCount $ arrowsLeft - 1,  Player) 
        
      
    (_,_,_) -> return ()
    
      
  where speedLimit = 7
        movePlayer v c@(Player, Velocity p, b :: Behavior) 
          | norm p > speedLimit = c
          | True = if b /= Plant then
                      (Player, Velocity $ p + v, b ) else
                      (Player, Velocity $ p + v, NoBehavior)
             
handle e = do
  liftIO $ print e
        
