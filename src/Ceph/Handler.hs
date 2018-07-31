module Ceph.Handler where

import Ceph.Components
import Ceph.Util
import Ceph.Entity.Projectile

import Apecs
import Apecs.Util
import Graphics.Gloss.Interface.IO.Game
-- import qualified SDL.Mixer as M
import Linear

mouseToWorld :: (Float,Float) -> Camera -> V2 Double
mouseToWorld (x,y) (Camera offset scale) = (/scale) <$> (V2 (realToFrac x) (realToFrac y))-offset

handle :: Event -> System World ()
handle (EventMotion mscreen) = do
  mpos <- mouseToWorld mscreen <$> get global
  offset <- (\(Camera o s) -> o ) <$> get global
  cmap $ \(Target a) -> Target (offset + mpos)
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
          cmapM_ $ \(Target o) -> cmap $ \(Player, Position p) -> Velocity $ 0.5 * normalize (o - p)
          cmap $ \(Dash w) -> Dash (-8.0)
        else return ()
      cmap $ \(Player) -> (Player, Attacking)
                                        
    (MouseButton RightButton,Up) -> do
      cmapM_ $ \c@(Player,Attacking, e) -> destroy e (Proxy :: Proxy Attacking)
    (e, f) -> return ()

  case (press, downup, modifiers) of
    (MouseButton LeftButton,Down,Modifiers Down Up Up) ->
       cmapM_ $ \(Player, Box pb) -> do
         cmapM_ $ removeProjectile pb
         cmap $ \(Player) -> (Player, Debug "arrow collected!")
    (MouseButton LeftButton,Up,Modifiers Down Up Up) ->
       cmapM_ $ \(Player, Box pb) -> do
         cmapM_ $ \(Player, Debug _, e) -> destroy e (Proxy :: Proxy Debug)
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> do
      cmap $ \(Player) -> (Player, Charging)

    (MouseButton LeftButton,Up,Modifiers _ _ _) -> do
      cmapM_ $ \(Position x, Velocity v, ProjCount arrowsLeft, Player) -> do
        Target t <- get global --cmapM_ $ \(Target t) -> do
        
        Charge c <- get global
        cmap $ \(Charge c) -> (Charge 0.01)
        cmap $ \(ProjCount x, Player) -> (Player, ProjCount $ x - 1)
        if arrowsLeft >= 1
          then do
          --cmapM_ $ \(Player,Resources _ p) -> if p == [] then return () else M.play $ head p
          shootArrow t x v c
          return () 
          else return ()
      

    (_,_,_) -> return ()
    
      
  where speedLimit = 7
        movePlayer v (Velocity p, Player) =
          (Player, Velocity $ p + v )-- if speedLimit > (norm p) then Velocity $ p + v else Velocity p)
handle e = do
  liftIO $ print e
