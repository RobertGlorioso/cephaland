{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Apecs
import Apecs.Util
import Apecs.Physics
import Apecs.Physics.Gloss
import Data.Ord hiding (Down)
import Data.List
import Control.Monad

import Graphics.Gloss.Juicy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Game as Game (shift)
import Linear
import qualified SDL
import qualified SDL.Mixer as M
import Enemy
import Sword
import Handler
import Data
import Init
import Util
import Projectile

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  M.openAudio M.defaultAudio 256
  w <- initWorld
  runSystem initGame w
  playIO (InWindow "seffyball" (640,480) (100,100)) (mixColors 0.1 0.9 white black) 59 w render handler stepper
  M.closeAudio
  M.quit
  SDL.quit
  where
    render w = do
      --updates HUD to show cooldown time for dash
      [x] <- runSystem (cmapM $ \(Dash w) -> return w) w
      if x < 8.0 then runSystem (rmap $ \(Dash w) -> Dash (w + 0.1) ) w else return ()
      newWorld <- runWith w $ do
        --get the camera to follow the player
        [a] <- cmapM $ \(Player, Position p) -> return p
        modifyGlobal $ \(Camera o s) -> (Camera a s)
        --correct angle for arrows
        rmap $ \(Velocity v, Angle t, Projectile) -> (Projectile, Angle $ vToRad v + pi /2)
        --updates display
        drawWorld
      --make the scene by combining the HUD and the current games display
      return $ Pictures [newWorld,Line [((-8),10),(x,10)]]
    handler event w = runSystem (handle event) w >> return w
    stepper dT w = go >> return w
      where go = runWith w (
              do
                --if the player moves, the target will also move
                [pp1] <- cmapM $ \(Player, Position p) -> return p
                --get the first player position, then run the physics step
                stepPhysics (1/60)
                --the second player position is expanded to get coordinates which will be used soon
                [pp2@(V2 x1 y1)] <- cmapM $ \(Player, Position p) -> return p
                --this updates the target
                rmap $ \(Target o) -> Target (o + pp2 - pp1)
                --this code exists for orienting the player and weapons
                [targetPos@(V2 x2 y2)] <- cmapM $ \(Target o) -> return o
                cimapM_ $ \(e, Player) -> do
                  isAttack <- exists $ cast e @Attacking
                  if isAttack
                    then cmapM_ $ \(Sword, Box sb) -> do
                      rmap $ showSword x1 x2 targetPos pp2
                      rmap $ killEnemy sb
                    else rmap hideSword

                --kill enemies that come in contact w/ projectiles
                cimapM $ \(e, (Projectile, Box pb)) -> rmap $ killEnemy pb
                
                rmap $ \(Player, Resources p _) -> (BodyPicture . scale 0.014 0.014 $ head p, Angle (vToRad $ targetPos - pp2))

                --updates all hit boxes to current position
                --could be removed if i figure out collision detection in the apecs-physics lib
                rmap $ \(Box (b, w, h), Position p) -> (Box (p, w, h))        
              )





-- old dumb stuff

infixl 7 <++
infixl 7 <+

(<++) :: IO [Convex] -> IO [Convex] -> IO [Convex] -- place two grids side by side
(<++) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  verts2 <- return $ concatMap vertices b'
  let size2 = maximum $ fmap (\(V2 s t) -> s) verts2
  return $ a' ++ fmap (Apecs.Physics.shift (V2 (size1 + size2) 0)) b'

(<+) :: IO [Convex] -> IO [Convex] -> IO [Convex] -- place two grids side by side
(<+) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  return $ a' ++ fmap (Apecs.Physics.shift (V2 size1 0)) b'

{--
randomGrid :: Int -> IO [Convex]
randomGrid 0 = block
randomGrid 1 = block <++ randomGrid 0
randomGrid n = block <+ randomGrid (n - 1)
block = do
  g <- randomRIO (1, 10 :: Int)
  a <- randomRIO (1, 10 :: Int)
  m <- randomRIO (1, 10 :: Int)
  n <- randomRIO (1, 10 :: Int)
  return $ gridLines (V2 (fromIntegral g) (fromIntegral a)) m n
--}
  
infixl 7 <++$   
infixl 7 <++*
infixl 7 <+*

(<++$) :: [Convex] -> [Convex] -> [Convex]
(<++$) a b = let
  verts1 = concatMap vertices a
  size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  verts2 = concatMap vertices b
  size2 = maximum $ fmap (\(V2 s t) -> s) verts2
  in a ++ fmap (Apecs.Physics.shift (V2 (size1 + size2) 0)) b

(<++*) :: System World [Convex] -> System World [Convex] -> System World [Convex] -- place two grids side by side
(<++*) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  verts2 <- return $ concatMap vertices b'
  let size2 = maximum $ fmap (\(V2 s t) -> s) verts2
  return $ a' ++ fmap (Apecs.Physics.shift (V2 (size1 + size2) 0)) b'

(<+*) :: System World [Convex] -> System World [Convex] -> System World [Convex] -- place two grids side by side
(<+*) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  return $ a' ++ fmap (Apecs.Physics.shift (V2 size1 0)) b'
