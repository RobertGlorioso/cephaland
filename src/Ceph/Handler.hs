{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Handler where

import Ceph.Components
import Ceph.Component.Projectile
import Ceph.Component.Player

import Apecs
--import Control.Concurrent
import Graphics.Gloss.Interface.IO.Game
import Linear

mouseToWorld :: (Float,Float) -> Camera -> V2 Float
mouseToWorld (x,y) (Camera o s) = o + ((V2 x y ) / pure s) 

handle :: Event -> System World ()
handle (EventResize (m1, m2)) = do
  modify global $ \(SB _) -> SB (V2 m1 m2)
handle (EventMotion mscreen) = do
  mpos <- mouseToWorld mscreen <$> get global
  cmap $ \(Target _) -> Target (mpos)
  
handle (EventKey press downup modifiers mscreen) = do
  case (press, downup) of
    (SpecialKey KeyRight, Down) -> 
      cmap $ movePlayer (V2 0.01 0)
    (Char 'd', Down) ->
      cmap $ movePlayer (V2 0.01 0)
    (SpecialKey KeyRight, Up) -> 
      cmap $ \case
        (Player1, Moving _) -> NoBehavior
        (Player1, b) -> b
    (Char 'd', Up) ->
      cmap $ \case
        (Player1, Moving _) -> NoBehavior
        (Player1, b) -> b
    (SpecialKey KeyLeft, Down) ->
      cmap $ movePlayer (V2 (-0.01) 0)
    (Char 'a', Down) ->
      cmap $ movePlayer (V2 (-0.01) 0)
    (SpecialKey KeyLeft, Up) ->
      cmap $ \case
        (Player1,Moving _) -> NoBehavior
        (Player1,b) -> b 
    (Char 'a', Up) ->
      cmap $ \case
        (Player1,Moving _) -> NoBehavior
        (Player1,b) -> b 
    (SpecialKey KeyUp, Down) -> 
      cmap $ movePlayer (V2 0 0.01)
    (Char 'w', Down) ->
      cmap $ movePlayer (V2 0 0.01)
    (SpecialKey KeyUp, Up) -> 
      cmap $ \case
        (Player1, Moving _) -> NoBehavior
        (Player1, b) -> b
    (Char 'w', Up) ->
      cmap $ \case
        (Player1, Moving _) -> NoBehavior
        (Player1, b) -> b
    (SpecialKey KeyDown, Down) -> 
      cmap $ movePlayer (V2 0 (-0.01))
    (Char 's', Down) ->
      cmap $ movePlayer (V2 0 (-0.01))
    (SpecialKey KeyDown, Up) -> 
      cmap $ \case
        (Player1, Moving _) -> NoBehavior
        (Player1, b) -> b
    (Char 's', Up) ->
      cmap $ \case
        (Player1, Moving _) -> NoBehavior
        (Player1, b) -> b
    (Char '=', Up) -> global `modify` \case
      (Beat 100 k) -> Beat 100 k
      (Beat j k) -> Beat (j+1) k
    (Char '-', Up) ->  global `modify` \case
      (Beat 0 k) -> Beat 0 k
      (Beat j k) -> Beat (j-1) k    
    (SpecialKey KeySpace, Up) -> do
      cmap $ \case
        (Player1, Carry, Position p) -> (Player1, NoBehavior, Position (p + pure 30.5))
        c -> c
    (SpecialKey KeySpace, Down) ->
      cmap $ \(Player1) -> (Player1, Carry)

    (Char '`', Up) -> do
      let toggleAuto Seek = NoBehavior
          toggleAuto _ = Seek
      cmap $ \(Player1, b) -> (Player1, toggleAuto b)        
    (e, f) -> return ()
    
  case (press, downup, modifiers) of
    (MouseButton LeftButton,Up,Modifiers Down Up Up) -> cmapM_ playerShootChain
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> cmap $ \case
      (Player1, Charge c _) -> (Player1, Charge c True)
      a -> a
   
    (MouseButton LeftButton,Up,Modifiers _ _ _) -> do
      cmapM_ playerShootArrow
    (MouseButton RightButton, Down, Modifiers _ _ _) -> do
      cmapM_ $ \case
        (Player1, Swinging) -> cmap $ \Player1 -> NoBehavior
        (Player1, _) -> playerSwinging
            
    (_,_,_) -> return ()
            
handle e = do
  liftIO $ print e
        
