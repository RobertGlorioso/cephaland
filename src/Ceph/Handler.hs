{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Handler where

import Ceph.Components
import Ceph.Util
import Ceph.Component.Projectile
import Ceph.Component.Player
import Ceph.Physics.Box

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
    
    (SpecialKey KeySpace, Up) -> do
      cmap $ \(Player1, b :: Behavior) -> (Player1, NoBehavior)
    (SpecialKey KeySpace, Down) ->
      cmap $ \(Player1) -> (Player1, Carry)
    (Char '`', Up) -> do
      let toggleAuto Seek = NoBehavior
          toggleAuto _ = Seek
      cmap $ \(Player1, b) -> (Player1, toggleAuto b)
    
                          
    (e, f) -> return () -- -- liftIO (print e >> print f >> return ())
    
  case (press, downup, modifiers) of
    (MouseButton LeftButton,Down,Modifiers Down Up Up) -> cmapM_ removeProjectile
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> cmap $ \case
      (Player1, Charge c _) -> (Player1, Charge c True)
      a -> a
   
    (MouseButton LeftButton,Up,Modifiers _ _ _) -> do
      cmapM_ playerShoot 
    (MouseButton RightButton, Down, Modifiers _ _ _) -> do
      playerSwinging
    (MouseButton RightButton,Up, Modifiers _ _ _) -> do
      cmapM_ $ \(Player1, e) -> e `destroy` (Proxy :: Proxy Linked) 
      --conceIf (\(Player1, a) -> a == Attack) (\Attack -> NoBehavior) 
      
    (_,_,_) -> return ()
            
handle e = do
  liftIO $ print e
        
