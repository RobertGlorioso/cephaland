{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Handler where

import Ceph.Components
import Ceph.Util
import Ceph.Component.Projectile
import Ceph.Component.Player

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
    (SpecialKey KeyRight, Down) -> 
      cmap $ movePlayer (V2 0.2 0)
    (Char 'd', Down) ->
      cmap $ movePlayer (V2 0.2 0)
    (SpecialKey KeyRight, Up) -> 
      cmap $ movePlayer (V2 0.2 0)  
    (Char 'd', Up) ->
      cmap $ movePlayer (V2 0.2 0)
    (SpecialKey KeyLeft, Down) ->
      cmap $ movePlayer (V2 (-0.2) 0)
    (Char 'a', Down) ->
      cmap $ movePlayer (V2 (-0.2) 0)
    (SpecialKey KeyLeft, Up) ->
      cmap $ movePlayer (V2 (-0.2) 0)
    (Char 'a', Up) ->
      cmap $ movePlayer (V2 (-0.2) 0)
    (SpecialKey KeyUp, Down) ->
      cmap $ movePlayer (V2 0 0.2) 
    (Char 'w', Down) ->
      cmap $ movePlayer (V2 0 0.2)
    (SpecialKey KeyUp, Up) ->
      cmap $ movePlayer (V2 0.0 0.2)
    (Char 'w', Up) ->
      cmap $ movePlayer (V2 0.0 0.2)
    (SpecialKey KeyDown, Down) ->
      cmap $ movePlayer (V2 0.0 (-0.2))
    (Char 's', Down) ->
      cmap $ movePlayer (V2 0.0 (-0.2))
    (MouseButton RightButton, Down) -> do
      cmapM_ $ playerDash                         
    (MouseButton RightButton,Up) -> do
      cmapM_ $ \c@(Player,Attacking, e) -> destroy e (Proxy :: Proxy Attacking)
    
    (SpecialKey KeySpace, Up) -> do
      cmap $ \(Player, b :: Behavior) -> (Player, NoBehavior)
    (SpecialKey KeySpace, Down) ->
      cmap $ \(Player) -> (Player, Carry)
                          
    (e, f) -> return () -- -- liftIO (print e >> print f >> return ())
    
  case (press, downup, modifiers) of
    (MouseButton LeftButton,Down,Modifiers Down Up Up) -> cmapM_ removeProjectile
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> cmap $ \case
      (Player, Charge c _) -> (Player, Charge c True)
      a -> a

    (MouseButton LeftButton,Up,Modifiers _ _ _) -> do
      cmapM_ playerShoot 
        
      
    (_,_,_) -> return ()
            
handle e = do
  liftIO $ print e
        
