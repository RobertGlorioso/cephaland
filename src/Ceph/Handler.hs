{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Ceph.Handler where

import Ceph.Components

import Apecs
import Control.Monad
import SDL.Input
import Linear

handle :: (Scancode -> Bool) -> System World ()
handle f = do 
  mousePos <- getAbsoluteMouseLocation
  global `set` (MCoordF (fromIntegral <$> mousePos) () :: MCoord)
  when (f ScancodeA) $ cmap $ \(Player1, Velocity v) -> Velocity (v - V2 0.2 0)
  when (f ScancodeW) $ cmap $ \(Player1, Dash c, Velocity v@(V2 _ vy)) -> 
    if c < 4 then (Dash c,Velocity v) else if vy < 0 then (Dash 0, Velocity (V2 0 (-4))) else (Dash 0, Velocity (v - V2 0 4))
  when (f ScancodeS) $ cmap $ \(Player1, Velocity v) -> Velocity (v + V2 0 0.2)
  when (f ScancodeD) $ cmap $ \(Player1, Velocity v) -> Velocity (v + V2 0.2 0)
  when (f ScancodeLeft) $ cmap $ \(Target  v) ->  (Target $ v - V2 2 0)
  when (f ScancodeUp) $ cmap $ \(Target  v) ->  (Target $ v - V2 0 2)
  when (f ScancodeDown) $ cmap $ \(Target  v) ->  (Target $ v + V2 0 2)
  when (f ScancodeRight) $ cmap $ \(Target  v) ->  (Target $ v + V2 2 0)
  
  when (f ScancodeF) $ do
    modify global $ \case 
      BoardControl Play l -> BoardControl Pause l
      BoardControl Pause l -> BoardControl Play l
                          
  when (f ScancodeSpace) $ do
    modify global $ \case 
      BoardControl p Locked -> BoardControl p Unlocked
      BoardControl p Unlocked -> BoardControl p Locked

  when (f ScancodeZ) $ do
    modify global $ \case
      MBoard v True -> MBoard v False :: Netitor
      MBoard v False -> MBoard v True :: Netitor

  when (f ScancodeMinus) $ do
    global `modify` \case
      (Beat 500 k) -> Beat 500 k
      (Beat j k) -> Beat (j+1) k
  when (f ScancodeEquals) $ do
    global `modify` \case
      (Beat 2 k) -> Beat 2 k
      (Beat j k) -> Beat (j-1) k
  {-- case (f <$> [Scancode1,Scancode2,Scancode3,Scancode4]) of
      [True,_,_,_] -> global `modify` \(SBoard a b c d) -> SBoard a a a a :: Sequencer
      [False,True,_,_] -> global `modify` \(SBoard a b c d) -> SBoard b b b b :: Sequencer
      [False,False,True,_] -> global `modify` \(SBoard a b c d) -> SBoard c c c c :: Sequencer
      [False,False,False,True] -> global `modify` \(SBoard a b c d) -> SBoard d d d d :: Sequencer
      _ -> return ()
  
  --}