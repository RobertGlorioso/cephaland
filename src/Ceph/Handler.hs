{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Ceph.Handler where

import Ceph.Components
import Ceph.Scene.Board
import Ceph.Component.Projectile
import Ceph.Component.Player

import Apecs
import Control.Monad
import SDL.Input
import Linear
import Foreign.C.Types

handle :: (Scancode -> Bool) -> System World ()
handle f = do 
  when (f ScancodeA) $ cmap $ \(Player1, Velocity v) -> Velocity (v - V2 0.2 0)
  when (f ScancodeW) $ cmap $ \(Player1, Velocity v) -> Velocity (v - V2 0 0.2)
  when (f ScancodeS) $ cmap $ \(Player1, Velocity v) -> Velocity (v + V2 0 0.2)
  when (f ScancodeD) $ cmap $ \(Player1, Velocity v) -> Velocity (v + V2 0.2 0)
  when (f ScancodeF) $ modify global $ \case 
                          Play -> Pause
                          Pause -> Play
                          
  when (f ScancodeSpace) $ modify global $ \case 
                          Locked -> Unlocked
                          Unlocked -> Locked
    --s <- get global :: System World (SCoordF ())
    --global `modify` (\b -> fillB (indAdj b s) :: Sequencer)
                                        
  when (f ScancodeMinus) $ do
    global `modify` \case
      (Beat 500 k) -> Beat 500 k
      (Beat j k) -> Beat (j+1) k
  when (f ScancodeEquals) $ do
    b :: Beat <- get global
    liftIO . writeFile "temp.txt" $ show b 
    global `modify` \case
      (Beat 2 k) -> Beat 2 k
      (Beat j k) -> Beat (j-1) k
  case (f <$> [Scancode1,Scancode2,Scancode3,Scancode4]) of
      [True,_,_,_] -> global `modify` \(SBoard a b c d) -> SBoard a a a a :: Sequencer
      [False,True,_,_] -> global `modify` \(SBoard a b c d) -> SBoard b b b b :: Sequencer
      [False,False,True,_] -> global `modify` \(SBoard a b c d) -> SBoard c c c c :: Sequencer
      [False,False,False,True] -> global `modify` \(SBoard a b c d) -> SBoard d d d d :: Sequencer
      _ -> return ()
  
  
{--
mouseToWorld :: (CDouble,CDouble) -> Camera -> V2 CDouble
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
    (Char 's',Up,Modifiers Down Up Up) -> (get global :: System World Sequencer) >>= saveBoard
    (MouseButton LeftButton,Up,Modifiers Down Up Up) -> cmapM playerShootChain
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> cmap $ \case
      (Player1, Charge c _) -> (Player1, Charge c True)
      a -> a
   
    (MouseButton LeftButton,Up,Modifiers _ _ _) -> cmapM_ playerShootArrow
    (MouseButton RightButton, Down, Modifiers _ _ _) -> cmapM_ $ \case
        (Player1, Swinging) -> cmap $ \Player1 -> NoBehavior
        (Player1, _) -> playerSwinging      
    (_,_,_) -> return ()
handle e = do
  liftIO $ print e
        
--}