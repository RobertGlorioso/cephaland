{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Ceph.Handler where

import Ceph.Components
import Ceph.Component.Levels
import Ceph.Jams
import Ceph.Util
import Data.Functor.Rep
import Data.List
import Euterpea (line, Music(..), qn, rest, chord)
import Apecs
import Ceph.Physics.Box
import Ceph.Scene.Camera
import Ceph.Scene.HUD
import Ceph.Scene.Board
import Ceph.Scene
import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import SDL (Rectangle(..), Point(..))
import SDL.Input
import Linear

handle :: (Scancode -> Bool) -> (MouseButton -> Bool) -> System World ()
handle keyed pressed = do 
  mousePos <- getAbsoluteMouseLocation
  let mpos r = let P v = fromIntegral <$> mousePos in Box (v,r,r)
  global `modify` (\mc -> mc {bx = mpos 1} :: MCoord)

  (sc :: SCoord, view, bdctrl, rend :: SDLRenderer) <- get global
  when (pressed ButtonLeft) $ do
    if (not $ aabb (mpos 1) (box 0 250 100)) then do
      cmapM_ $
        \(mb@(MBoard ms b)) -> do
          let sel = index 
                (mb { mboxes = (\(Box (bx, w, h), e) -> (Box (applyView view (Position bx) 0, w, h), e)) <$> ms })
                (MCoordF (mpos 1) (Entity 1))
          global `set` MCoordF (mpos 1) sel
          
          when (charge bdctrl >= 15) $ do
            modify sel
              $ \case
                  Sing -> NoBehavior
                  NoBehavior -> Sing
                  b -> b
            global `set` bdctrl { charge = 0 } 
      else return ()
    cmapM_ $ \(Player1, p) -> saveGridCell p

  when (pressed ButtonRight) $ do
    cmapM_ $
      \(mb@(MBoard ms b)) -> do
        let sels = snd <$> filter (aabb (mpos 10) . fst) 
              ((\(Box (bx, w, h), e) -> (Box (applyView view (Position bx) 0, w, h), e)) <$> ms )
        global `set` MCoordF (mpos 10) (head sels)
        
        when (charge bdctrl >= 15) $ do
          flip mapM_ sels 
            (`modify`
              \case
                Sing -> NoBehavior
                NoBehavior -> Sing
                b -> b
            )
          global `set` bdctrl { charge = 0 }     
    cmapM_ $ \(Player1, p) -> saveGridCell p
    
    
  cmapM_ $ \pl@(Player1, Velocity v@(V2 vx vy), Dash c, e, beh :: Behavior) -> do
    when (c > 1 && keyed ScancodeA) $ e `set` (Player1, Velocity (v + V2 (-0.7) 0), Dash $ c / 2, NoBehavior)
    when (c > 2 && keyed ScancodeS) $ e `set` (Player1, Velocity (v + V2 0 0.2), Dash $ c / 2, NoBehavior) 
    when (c > 1 && keyed ScancodeD) $ e `set` (Player1, Velocity (v + V2 0.7 0), Dash $ c / 2, NoBehavior)
    when (c > 2 && keyed ScancodeW) $ e `set` if vy > 1 then (Player1, Velocity (V2 vx (-2.5)), Dash 1, NoBehavior) else (Player1, Velocity (v - V2 0 2), Dash 0, NoBehavior)  
  
  cmapM_ $ \(Target v, e) -> do 
    when (norm v < 170) $ do
      when (keyed ScancodeLeft) $ e `set` (Target $ v - V2 7 0)
      when (keyed ScancodeUp) $ e `set` (Target $ v - V2 0 7)
      when (keyed ScancodeDown) $ e `set` (Target $ v + V2 0 7)
      when (keyed ScancodeRight) $ e `set` (Target $ v + V2 7 0)
    when (norm v >= 170) $ do
      e `set` (Target $ pure 0.98 * v)
    when (norm v <= 50) $ do
      e `set` (Target $ pure 1.1 * v)
      
  when (keyed ScancodeF && charge bdctrl >= 15) $ do
    modify global $ \case 
      BoardControl d Play l i b -> BoardControl d Pause l 0 b
      BoardControl d Pause l i b -> BoardControl d Play l 0 b
                          
  {--when (keyed ScancodeM && charge bdctrl >= 200) $ do
    outM <- flip cfoldM (rest qn) $ \a (f :: Sequencer) ->  return . (a :=:) . line . fmap chord . (fmap.fmap) song . fToList =<< (mapM.mapM) (get) f
    
    newMid <- liftIO $ async $ midiWrite "out.mid" outM
    ns <- liftIO $ wait newMid

    cmap (\(Player1, Position p) -> (SFXResources ns outM (SpriteColor $ pure 0)))
    set global bdctrl { charge = 0 }--}

  when (keyed ScancodeSpace && charge bdctrl >= 15) $ do
    modify global $ \case 
      BoardControl d p Locked i b -> BoardControl d p Unlocked 0 b
      BoardControl d p Unlocked i b -> BoardControl d p Locked 0 b
  
  when (charge bdctrl >= 5 && status bdctrl == Pause && keyed ScancodeN) $ do
    set global bdctrl { status = Play }
    modify global $ \(Beat m _) -> Beat m m
    cmapM_ $ \bd -> playBoard bd (succCycle sc)
    incrementBeat
    modify global $ \(Beat m _) -> Beat m 1
    set global bdctrl { status = Pause, charge = 0 }
  
  let seqRowSwitches = keyed <$> [Scancode1,Scancode2,Scancode3,Scancode4]
  when (charge bdctrl >= 15 && any id seqRowSwitches) $ do
    let switched = fmap fst $ filter (snd) $ zip ([S1,S2,S3,S4]) seqRowSwitches
    set global bdctrl { charge = 0, playback = union (playback bdctrl \\ switched) (switched \\ playback bdctrl) }
  
  -- when (keyed ScancodeZ) $ do
  --   modify global $ \case
  --     MBoard v True -> MBoard v False :: Netitor
  --     MBoard v False -> MBoard v True :: Netitor

  when (keyed ScancodeMinus) $ do
    global `modify` \case
      (Beat 500 k) -> Beat 500 k
      (Beat j k) -> Beat (j+1) k

  when (keyed ScancodeEquals) $ do
    global `modify` \case
      (Beat 2 k) -> Beat 2 k
      (Beat j k) -> Beat (j-1) k
