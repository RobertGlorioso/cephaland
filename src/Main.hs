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
import Ceph.Util
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
  set global ( Camera 0 3.3
             , Gravity $ V2 0 (-0.01)
             , Beat 15 0)  
  -- make some euterpea sounds
  let e1 = e 3 wn 
      e2 = fs 3 wn 
      e3 = gs 3 wn
      m1 = transpose 7 $ e1
      m2 = transpose 7 $ e2
      m3 = transpose 7 $ e3
      o1 = instrument ElectricGrandPiano $ m1 :=: m2 :=: m3
      o2 = transpose 2 o1
      o3 = transpose 5 o1
      n1 = instrument HammondOrgan $ e1 :=: e2 :=: e3
      n2 = transpose 2 n1
      n3 = transpose 5 n1
      j1 = Euterpea.line $ concat $ replicate 4 [n1, n2]
      j2 = Euterpea.line $ concat $ replicate 4 [n3, n2]
      j3 = Euterpea.line $ concat $ replicate 4 [n3, n1]
      l1 = Euterpea.line $ concat $ replicate 4 [o2, o1]
      l2 = Euterpea.line $ concat $ replicate 4 [o2, o3]
      l3 = Euterpea.line $ concat $ replicate 4 [o1, o3]
      u1 = tempo 0.1 $ t1 42 AcousticGrandPiano
      u2 = tempo 0.5 $ t1 14 ElectricGuitarJazz
      u3 = tempo 0.2 $ t1 2 OverdrivenGuitar
      u4 = tempo 0.1 $ t1 5 Banjo
      am = Euterpea.line <$> fpow ( take 36 $ cycle [j1,j2,j3,l1,l2,l3] ) 6 (take 6) (drop 6) -- [u1,u2,u3,u4]
      
      b1 = perc HiMidTom sn
      b2 = perc HiBongo sn
      b3 = perc HighFloorTom sn
      b4 = perc HiWoodBlock sn
      b5 = perc BassDrum1 sn
      b6 = perc LowMidTom sn
      b7 = perc LowBongo sn
      b8 = perc LowFloorTom sn
      b9 = perc LowWoodBlock sn
      b10 = perc Cowbell sn
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
  
  
  blck3 <- liftIO  $ zip4 <$> randomDonutBox 1000 600 900 <*> randomDonutBox 1000 600 400 <*> replicateM 1000 (randomRIO (20,100 :: Float)) <*> replicateM 500 (randomRIO (20,100 :: Float))

  targ <- newEntity (Target 0)
  let bl (r,s,g,a) =
        --if (g > 70 && a < 30) || ( g < 30 && a > 70) then
          newEntity ((Wall,Wall1)
                    , Position (V2 r s)
                    , Angle ( (g + a) / 21 )
                    , Velocity 0
                    , box ( V2 r s ) (0.3*g) (0.3*a)
                    , BodyPicture $ Scale (0.02 * g) (0.02 * a) plante
                    ) 
          --else newEntity ()
          
      ch = newEntity (Chain
                     , Weapon
                     , NoBehavior
                     , Angle 0
                     , Position 0
                     , Velocity 0
                     ,( box 0 0.05 0.05
                     , Song (rest 0)
                     , BodyPicture $ Pictures
                       [Line [(0,0), (10,0)]
                       ,Scale (0.03) (0.03) plante]
                     ))
      chains [] _ = return ()
      chains (last2:last:[]) (d:ds) = last `set` (SFXResources [d] [], Linked last2 targ)
      chains (prev:cur:next:rest) (d:ds) = do
        cur `set` (SFXResources [d] [],Linked prev next)
        chains (cur:next:rest) $ ds ++ [d]
  --chains to arms     
  mapM_ bl blck3
  blk1 <- bl (0,40,150,12)
  blk1 `set` (Angle (pi/3), Seek)

  --dummy
  newEntity (( Position (V2 0 50)
             , 0 :: Velocity
             , Angle 0
             , BodyPicture $ Scale (1/4) (1/4) (Pictures [octo,color red $ Circle 1]) 
             , Box (0, 1, 1))
            , (Dummy, ProjCount 30, Health 99, Dash 0)
            , (Plant, Charge 0.01 False))
  
  sword cig
  hp1 <- harpoon arw
  hp2 <- harpoon arw
  pl <- player octo n3
  chns <- replicateM 10 ch
  --chns2 <- replicateM 10 ch
  --chns3 <- replicateM 7 ch
  --chns4 <- replicateM 15 ch
  --chns5 <- replicateM 4 ch
  chains (pl:chns) dm
  --chains (pl:chns2) dm
  --chains (pl:chns3) dm
  --chains (pl:chns4) dm 
  --chains (pl:chns5) dm

  liftIO . print $ length am
  mapM_ (newArrow arw) $ concat . replicate 10 $ zip am cm
  mapM_ (newBullet bults) $ concat . replicate 10 $ zip bm dm
            
  mapM_ (enemy squid) $ zip am cm 
  newEntity ( Grid $ fromList [ (0, fromList [(0,())] ) ] )

  
  return ()

