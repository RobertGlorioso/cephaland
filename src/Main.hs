{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Ceph.Scene
import Ceph.Util
import Ceph.Components
import Ceph.Physics
import Ceph.Physics.Box
import Ceph.Component.Enemy
import Ceph.Component.Player
import Ceph.Component.Projectile
import Ceph.Component.Levels
import Ceph.Component.Weapon
import Ceph.Handler
import Ceph.Jams
import Apecs
import Data.List (zip4)
import Data.Bool
import Options.Applicative
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import System.Random
import Data.Monoid
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
  playIO (InWindow "CEPH" (640,480) (100,100)) (mixColors 0.5 0.5 black violet) 60 w (render o) (\e w -> runSystem (handle e) w >> return w) (stepper)
  M.closeAudio
  M.quit
  S.quit

parseopts = GameOpts <$> switch
  ( long "debug"
    <> help "Turn on in-game debug hud" )

initGame :: System World ()
initGame = do
  
  set global ( Camera 0 8.0
             , Gravity $ V2 0 (-0.0095)
             , Beat 16 0
             
             , mempty :: Sequencer
             , mempty :: SCoord
             )  
  -- make some euterpea sounds
  let e1 = e 3 qn 
      e2 = fs 3 qn 
      e3 = gs 3 qn
      m1 = transpose 7 $ e1
      m2 = transpose 7 $ e2
      m3 = transpose 7 $ e3
      o1 = instrument HammondOrgan $ m1 :=: m2 :=: m3
      o2 = transpose 2 o1
      o3 = transpose 5 o1
      n1 = instrument HammondOrgan $ e3 :=: e2 :=: e1
      n2 = transpose 2 n1
      n3 = transpose 5 n1
      j1 = [n1, n2]
      j2 = [n3, n2]
      j3 = [n3, n1]
      l1 = [o2, o1]
      l2 = [o2, o3]
      l3 = [o1, o3]
      u1 = tempo 0.1 $ t1 42 AcousticGrandPiano
      u2 = tempo 0.5 $ t1 14 ElectricGuitarJazz
      u3 = tempo 0.2 $ t1 2 OverdrivenGuitar
      u4 = tempo 0.1 $ t1 5 Banjo
      am = [o1,o2,o3,n1,n2,n3] -- Euterpea.chord <$> fpow ( take 36 $ cycle [j1,j2,j3,l1,l2,l3] ) 6 (take 6) (drop 6) -- [u1,u2,u3,u4]

      b0 = rest sn
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
      bm = let b = [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10] in b ++ [a :=: a' | a<-take 5 b, a'<-drop 5 b]
      
  cm <- mapM ( M.decode . makeByteMusic ) am :: System World [M.Chunk]
  dm <- mapM ( M.decode . makeByteMusic ) bm 


  --read in sprites
  
  let handlePic = maybe
        (error "img not loading")
        return
  
  plante <- liftIO $ handlePic =<< loadJuicy "./resource/image/coral1.png" 
  squid <- liftIO $ handlePic =<< loadJuicy "./resource/image/squid1.png"
  arw <- liftIO $ handlePic =<< loadJuicy "./resource/image/arrow.png"
  octo <- liftIO $ handlePic =<< loadJuicy "./resource/image/octo2.png"
  bults <- liftIO $ mapM (\b -> handlePic =<< loadJuicy b) ["./resource/image/bullet1.png","./resource/image/bullet2.png","./resource/image/bullet3.png"]

  --moving target for grappling and shooting
  targ <- newEntity (Target 0,Box (0,0,0),Angle 0,Position 0)
  let bl (r,s,g,a) (d,o) = do
          newEntity ((Wall,Wall1)
                    , Position (V2 r s)
                    , Angle ( if g > a then 2*pi - (g + a) / 3200 else (g + a) / 3200 )
                    , Velocity 0
                    , box (V2 r s) (0.3*g) (0.3*a)
                    , SFXResources [d] []
                    , Song o
                    , BodyPicture $ Scale (0.02 * g) (0.02 * a) plante
                    ) 
          
  walls <- liftIO $
    zip4 <$>
      randomDonutBox 200 600 900
      <*> randomDonutBox 200 600 400
      <*> replicateM 200 (randomRIO (20,100 :: Float))
      <*> replicateM 200 (randomRIO (20,100 :: Float))
  mapM_ (uncurry bl) $ zip walls (zip dm bm)
  
  pl <- player octo n3
  chns <- replicateM 10 (chain plante)
  chains (pl:chns) targ

  mapM_ (newArrow arw) $ concat . replicate 10 $ zip am cm
  mapM_ (newBullet bults) $ concat . replicate 10 $ zip am cm
            
  mapM_ (enemy squid) $ take 10 $ zip bm dm 

  newEntity ( Grid mempty )

  cmap (\bx -> bool Out In $ aabb bx (Box (0, 400, 400)))

  randomizeGridCell 0  

  return ()

