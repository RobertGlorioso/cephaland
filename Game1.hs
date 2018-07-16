{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Apecs
import Apecs.Util
import Data.Ord hiding (Down)
import Data.List
import Control.Monad
import Data.Proxy
import Graphics.Gloss.Juicy
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle        (radToDeg)
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
import Physics

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  M.openAudio M.defaultAudio 256
  w <- initWorld
  runSystem initGame w
  render w
  stepper (1/60) w
  playIO (InWindow "seffyball" (640,480) (100,100)) (mixColors 0.1 0.9 white black) 60 w render handler stepper
  M.closeAudio
  M.quit
  SDL.quit

render w = do
      
      movableObjs <- runWith w $ getAll :: IO [(Position, Angle, BodyPicture)]

      pics <- mapM (\((Position (V2 x y), Angle theta, BodyPicture pic)) -> return . Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic) movableObjs

      [(Player, ProjCount i)] <- runWith w $ getAll
      
      runWith w $ do
        --updates cooldown time for dash
        cmapM_ $ \(Dash x) -> if x < 8.0 then cmap $ \(Dash x') -> Dash (x' + 0.1) else return ()

        updateCamera
        
        view <- get global :: System World Camera
        newWorld <- return . applyView view . mconcat $ pics
        
        --make the scene by combining the HUD and the current world
        Dash x <- get global :: System World Dash
        return $ Pictures [newWorld,
                           Translate 200 200 $ Scale 4 4 $ Line [((-8),10),(x,10)],
                           Translate 160 200 $ Scale 0.1 0.1 $ Text $ show i]

handler event w = runSystem (handle event) w >> return w

stepper dT w = go >> return w
      where
        go = runWith w (
          do
            --if the player moves, the target will also move
            cmapM_ $ \(Player, Position p1) -> do
              stepPhysics (1/60) --get the first player position, then run the physics step
              cmapM_ $ \(Player, Position p2@(V2 x1 y1)) -> do
                cmap $ \(Target o) -> Target (o + p2 - p1) --this updates the target
                cmapM_ $ \(Target tp@(V2 x2 y2)) -> do
          
                  cmapM_ $ \(Player, e) -> do
                    isAttack <- exists e (Proxy :: Proxy Attacking)
                    if isAttack
                      then cmapM_ $ \(Sword, Box sb) -> do
                        cmap $ showSword x1 x2 tp p2
                        cmap $ killEnemy sb
                      else cmap hideSword

                  cmapM_ $ \(Projectile, Box pb) -> cmap $ killEnemy pb
                  cmap $ \(Player) -> (Angle (vToRad $ p2 - tp))
            --cmap $ \(Player, Resources p _) -> (BodyPicture $ head p, Angle (vToRad $ targetPos - pp2))
            playerBoxBound)

      



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
  return $ a' ++ fmap (Util.shift (V2 (size1 + size2) 0)) b'

(<+) :: IO [Convex] -> IO [Convex] -> IO [Convex] -- place two grids side by side
(<+) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  return $ a' ++ fmap (Util.shift (V2 size1 0)) b'

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
  in a ++ fmap (Util.shift (V2 (size1 + size2) 0)) b

(<++*) :: System World [Convex] -> System World [Convex] -> System World [Convex] -- place two grids side by side
(<++*) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  verts2 <- return $ concatMap vertices b'
  let size2 = maximum $ fmap (\(V2 s t) -> s) verts2
  return $ a' ++ fmap (Util.shift (V2 (size1 + size2) 0)) b'

(<+*) :: System World [Convex] -> System World [Convex] -> System World [Convex] -- place two grids side by side
(<+*) a b = do
  a' <- a
  b' <- b
  verts1 <- return $ concatMap vertices a'
  let size1 = maximum $ fmap (\(V2 s t) -> s) verts1
  return $ a' ++ fmap (Util.shift (V2 size1 0)) b'
