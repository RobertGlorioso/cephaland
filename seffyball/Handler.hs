module Handler where

import Apecs
import Apecs.Util
import Apecs.Physics
import Apecs.Physics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified SDL.Mixer as M
import Linear
import Projectile
import Data
import Util 

handle :: Event -> System World ()
handle (EventMotion mscreen) = do
  mpos <- mouseToWorld mscreen <$> getGlobal
  offset <- (\(Camera o s) -> o ) <$> getGlobal
  rmap $ \(Target a) -> Target (offset + mpos)
handle (EventKey press downup modifiers mscreen) = do
  case (press, downup) of
    (SpecialKey KeyRight, Down) ->
      rmap $ movePlayer (V2 0.25 0)
    (Char 'd', Down) ->
      rmap $ movePlayer (V2 0.25 0)
    (SpecialKey KeyRight, Up) ->
      rmap $ movePlayer (V2 0.15 0)
    (Char 'd', Up) ->
      rmap $ movePlayer (V2 0.15 0)
    (SpecialKey KeyLeft, Down) ->
      rmap $ movePlayer (V2 (-0.25) 0)
    (Char 'a', Down) ->
      rmap $ movePlayer (V2 (-0.25) 0)
    (SpecialKey KeyLeft, Up) ->
      rmap $ movePlayer (V2 (-0.15) 0)
    (Char 'a', Up) ->
      rmap $ movePlayer (V2 (-0.15) 0)
    (SpecialKey KeyUp, Down) ->
      rmap $ \(Velocity (V2 x _), Player) -> (Player, Velocity (V2 x 3.4) )
    (Char 'w', Down) ->
      rmap $ \(Velocity (V2 x _), Player) -> (Player, Velocity (V2 x 3.4) )
    (SpecialKey KeyUp, Up) ->
      rmap $ movePlayer (V2 0.0 1.8)
    (Char 'w', Up) ->
      rmap $ movePlayer (V2 0.0 1.8)
    (SpecialKey KeyDown, Down) ->
      rmap $ \(Position x, Player) -> (Player, Position $ x + V2 0.0 (-0.07))
    (Char 's', Down) ->
      rmap $ \(Position x, Player) -> (Player, Position $ x + V2 0.0 (-0.07))
    (MouseButton RightButton, Down) -> do
      [coolDown] <- cmapM $ \(Dash w) -> return w
      if coolDown >= 8.0
      then do
        [targetPos] <- cmapM $ \(Target o) -> return o
        rmap $ \(Player, Position p) -> Velocity $ 4 * normalize (targetPos - p)
        rmap $ \(Dash w) -> Dash (-8.0)
      else return ()
      rmap $ \(Player) -> (Player, Attacking)
                                        
    (MouseButton RightButton,Up) -> do
      rmap' $ \(Player,Attacking) -> Safe Nothing :: Safe Attacking
    (e, f) -> return ()
  case (press, downup, modifiers) of
    (MouseButton LeftButton,Down,Modifiers Down Up Up) ->
       cmapM_ $ \(Player, Box pb) -> do
         --cmapM_ $ \(Projectile, Box pBox) -> liftIO.print $ aabb pb pBox
         rmap $ removeProjectile pb
    (MouseButton LeftButton,Down,Modifiers _ _ _) -> do
      [t] <- cmapM $ \(Target p) -> return p
      [(x,v)] <- cmapM $ \(Position p, Velocity q, Player) -> return (p,q)
      [arrowsLeft] <- cmapM $ \(ProjCount x, Player) -> do
                        rmap $ \(ProjCount x, Player) -> (Player, ProjCount $ x - 1)
                        return x
      if arrowsLeft >= 1
        then do
          cmapM $ \(Player,Resources c [p]) -> M.play p
          shootArrow t x v >> return ()
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
