module Handler where

import Apecs
import Apecs.Util
import Graphics.Gloss.Interface.IO.Game
import qualified SDL.Mixer as M
import Linear
import Projectile
import Data
import Util 

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
      cmap $ movePlayer (V2 0.02 0)
    (Char 'd', Up) ->
      cmap $ movePlayer (V2 0.02 0)
    (SpecialKey KeyLeft, Down) ->
      cmap $ movePlayer (V2 (-0.05) 0)
    (Char 'a', Down) ->
      cmap $ movePlayer (V2 (-0.05) 0)
    (SpecialKey KeyLeft, Up) ->
      cmap $ movePlayer (V2 (-0.02) 0)
    (Char 'a', Up) ->
      cmap $ movePlayer (V2 (-0.02) 0)
    (SpecialKey KeyUp, Down) ->
      cmap $ \(Velocity (V2 x _), Player) -> (Player, Velocity (V2 x 0.04) )
    (Char 'w', Down) ->
      cmap $ \(Velocity (V2 x _), Player) -> (Player, Velocity (V2 x 0.04) )
    (SpecialKey KeyUp, Up) ->
      cmap $ movePlayer (V2 0.0 0.02)
    (Char 'w', Up) ->
      cmap $ movePlayer (V2 0.0 0.02)
    (SpecialKey KeyDown, Down) ->
      cmap $ movePlayer (V2 0.0 (-0.02))
      --cmap $ \(Position x, Player) -> (Player, Position $ x + V2 0.0 (-0.07))
    (Char 's', Down) ->
      cmap $ movePlayer (V2 0.0 (-0.02))
      --cmap $ \(Position x, Player) -> (Player, Position $ x + V2 0.0 (-0.07))
    (MouseButton RightButton, Down) -> do
      cmapM_ $ \(Dash w) -> 
        if w >= 8.0
        then do
          cmapM_ $ \(Target o) -> cmap $ \(Player, Position p) -> Velocity $ normalize (o - p)
          cmap $ \(Dash w) -> Dash (-8.0)
        else return ()
      cmap $ \(Player) -> (Player, Attacking)
                                        
    (MouseButton RightButton,Up) -> do
      cmap $ \c@(Player,Attacking) -> Just c
    (e, f) -> return ()

  case (press, downup, modifiers) of
    (MouseButton LeftButton,Down,Modifiers Down Up Up) ->
       cmapM_ $ \(Player, Box pb) -> do
         cmap $ removeProjectile pb
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> do
      cmapM_ $ \(Target t) -> do 
        cmapM_ $ \(Position x, Velocity v, Player) -> do
          cmapM_ $ \(ProjCount arrowsLeft, Player) -> do
            cmap $ \(ProjCount x, Player) -> (Player, ProjCount $ x - 1)
            if arrowsLeft >= 1
              then do
              cmapM_ $ \(Player,Resources _ [p]) -> M.play p
              shootArrow t x v
              return () 
              else return ()
            return ()
    (e, f, g) -> return () 
  where speedLimit = 7
        movePlayer v (Velocity p, Player) =
          (Player, if speedLimit > ((\(V2 a b) ->  a*a + b*b) p)
                   then Velocity $ p + v
                   else Velocity p)
handle e = do
  liftIO $ print e
