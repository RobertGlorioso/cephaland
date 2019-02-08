{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Ceph.Scene
import Ceph.Components
import Ceph.Physics
import Ceph.Physics.Box
import Ceph.Component.Enemy
import Ceph.Component.Weapon
import Ceph.Component.Player
import Ceph.Component.Projectile
import Ceph.Handler
import Ceph.Jams

import Apecs
import Data.List (zip4)
import Options.Applicative
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import System.Random
import Data.Monoid
import Data.IntMap (fromList)
import Linear (V2(..))
import qualified SDL.Mixer as M
import qualified SDL as S
import Euterpea

main :: IO ()
main = do
  S.initialize [S.InitAudio]
  M.openAudio M.defaultAudio 256
  w <- initWorld
  let opts = info (parseopts <**> helper)
        $ fullDesc
          <> progDesc "The Ceph Game"
          <> header "--debug turns on debugging" 
  o <- liftIO (execParser opts)
  runSystem (initGame) w
  render o w
  stepper (1/60) w
  playIO (InWindow "CEPH" (640,480) (100,100)) (mixColors 0.1 0.9 blue black) 60 w (render o) (\e w -> runSystem (handle e) w >> return w) (stepper)
  M.closeAudio
  M.quit
  S.quit

parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )

initGame :: System World ()
initGame = do
  set global ( Camera 0 2
             , Gravity $ V2 0 (-0.02)
             , Beat 15 0)  
  -- make some euterpea sounds
  let e1 = e 4 wn 
      e2 = fs 4 wn 
      e3 = gs 4 wn
      m1 = transpose 7 $ e1
      m2 = transpose 7 $ e2
      m3 = transpose 7 $ e3
      o1 = instrument Flute $ m1 :=: m2 :=: m3
      o2 = transpose 2 o1
      o3 = transpose 5 o1
      n1 = transpose (-12) $ instrument HammondOrgan $ e1 :=: e2 :=: e3
      n2 = transpose 2 n1
      n3 = transpose 5 n1
      j1 = Euterpea.chord $ concat $ replicate 4 [n1,o2]
      j2 = Euterpea.chord $ concat $ replicate 4 [n3,o2]
      j3 = Euterpea.chord $ concat $ replicate 4 [n3,o1]
      l1 = Euterpea.chord $ concat $ replicate 4 [n2,o1]
      l2 = Euterpea.chord $ concat $ replicate 4 [n2,o3]
      l3 = Euterpea.chord $ concat $ replicate 4 [n1,o3]
      u1 = tempo 0.1 $ t1 42 AcousticGrandPiano
      u2 = tempo 0.5 $ t1 14 ElectricGuitarJazz
      u3 = tempo 0.2 $ t1 2 OverdrivenGuitar
      u4 = tempo 0.1 $ t1 5 Banjo
      am = [j1,j2,j3,l1,l2,l3] -- [u1,u2,u3,u4]
      
      b1 = perc HiMidTom sn
      b2 = perc HiBongo sn
      b3 = perc HighFloorTom sn
      b4 = perc HiWoodBlock sn
      b5 = perc BassDrum1 sn
      b6 = perc LowMidTom sn
      b7 = perc LowBongo sn
      b8 = perc LowFloorTom sn
      b9 = perc LowWoodBlock sn
      b10 = perc LongWhistle sn
      bm = [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10]
     
  cm <- mapM ( M.decode . makeByteMusic ) am :: System World [M.Chunk]
  dm <- mapM ( M.decode . makeByteMusic ) bm :: System World [M.Chunk] 
  --read in sprites
  let handlePic = maybe
        (error "img not loading")
        return
  
  cig <- liftIO $ handlePic =<< loadJuicy "./resource/image/sword.png"
  hrpn <- liftIO $ handlePic =<< loadJuicy "./resource/image/harpoon.png"
  plante <- liftIO $ handlePic =<< loadJuicy "./resource/image/coral1.png" 
  grund <- liftIO $ handlePic =<< loadJuicy "./resource/image/ground.png"
  squid <- liftIO $ handlePic =<< loadJuicy "./resource/image/squid1.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo2.png"
  bults <- liftIO $ mapM (\b -> handlePic =<< loadJuicy b) ["./resource/image/bullet1.png","./resource/image/bullet2.png","./resource/image/bullet3.png"]
  
  
  blck3 <- liftIO  $ zip4 <$> randomDonutBox 1000 600 900 <*> randomDonutBox 1000 600 400 <*> replicateM 1000 (randomRIO (20,30 :: Float)) <*> replicateM 500 (randomRIO (20,30 :: Float))

  targ <- newEntity (Target 0, Position 0)
  let bl (r,s,g,a) =
        newEntity (Wall
                  , Position (V2 r s)
                  , Angle 0
                  , Velocity 0
                  , box ( V2 r s ) g a
                  , BodyPicture $ Scale (0.05 * g) (0.05 * a) plante
                  )
      ch = newEntity (Chain
                     , Weapon
                     , NoBehavior
                     , Angle 0
                     , Position 0
                     , Velocity 0
                     ,( box 0 0.1 0.1
                     , Song (rest 0)
                     , BodyPicture $ Pictures
                       [Line [(0,0), (10,0)]
                       ,Scale (0.1) (0.1) plante]
                     ))
      chains [] _ = return ()
      chains (last2:last:[]) (d:ds) = last `set` (Resources [] [d], Linked last2 targ)
      chains (prev:cur:next:rest) (d:ds) = do
        cur `set` (Resources [] [d],Linked prev next)
        chains (cur:next:rest) $ ds ++ [d]
        
  mapM_ bl blck3
  sword cig
  harpoon hrpn
  pl <- player octo n3
  chns <- replicateM 10 ch
  --chns2 <- replicateM 25 ch
  chns3 <- replicateM 7 ch
  --chns4 <- replicateM 15 ch
  chns5 <- replicateM 4 ch
  chains (pl:chns) dm
  --chains (pl:chns2) dm
  chains (pl:chns3) dm
  --chains (pl:chns4) dm 
  chains (pl:chns5) dm
  
  mapM_ (newArrow arw) $ concat . replicate 10 $ zip am cm
  mapM_ (newBullet bults) $ concat . replicate 10 $ zip am cm
            
  mapM_ (enemy squid) $ zip bm dm 
  newEntity ( Grid $ fromList [ (0, fromList [(0,())] ) ] )
  return ()

