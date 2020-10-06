{-# LANGUAGE ViewPatterns #-}

module Ceph.Component.Wall where
import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box
import System.Random
import Foreign.C.Types
import qualified SDL as S
import Apecs
import Linear

jellyWall :: Entity -> Txtr -> System World Entity
jellyWall ent txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) = do
  newEntity ((Wall,Jelly)
          , Position 0 
          , Angle 0 
          , Velocity 0
          , NoBehavior
          , End ent
          , box (0) (x / 2) (y / 2)
          , txtr 
          )

nonWall :: (CDouble,CDouble,CDouble,CDouble) -> SFXResources -> Txtr -> System World Entity
nonWall (r,s,g,a) sfx@(SFXResources _ n _) txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) = do
  newEntity (Wall
          , Position (V2 r s)
          , Angle ( if g > a then 2*pi - (g + a) else (g + a) )
          , Velocity 0
          , NoBehavior
          , box (V2 r s) (x / 2) (y / 2)
          , (sfx
            , getInst n
            , txtr 
          ))

floorWall :: (CDouble,CDouble) -> SFXResources -> Angle -> Txtr -> System World Entity
floorWall (r,s) sfx n txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) = do
  ran <- liftIO $ newStdGen
  newEntity ((Wall,Floor)
            , Position (V2 r s)
            , n
            , Velocity 0
            , box (V2 r s) (x / 2) (y / 2)
            , sfx
            , txtr 
            ) 

oneWayWall :: (CDouble,CDouble) -> SFXResources -> Txtr -> System World Entity
oneWayWall (r,s) sfx txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) = do
  newEntity ((Wall,OneWayWall)
            , Position (V2 r s)
            , Angle (0)
            , Velocity 0
            , box (V2 r s) (x / 2) (y / 2)
            , sfx
            , txtr
            ) 

newWall :: S.Renderer -> FilePath -> Wall -> (CDouble,CDouble,CDouble,CDouble) -> SFXResources -> System World Entity
newWall r txtrFile w (b,c,g,a) s = do
  texture@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) <- liftIO $ loadTxtr r txtrFile
  newEntity ((Wall,w)
            , Position (V2 b c)
            , NoBehavior
            , Angle ( if g > a then 2*pi - (g + a) else (g + a) )
            , Velocity 0
            , box (V2 b c) (x / 2) (y / 2)
            , s
            , texture
            )

hardWall :: Wall -> (CDouble,CDouble,CDouble,CDouble) -> SFXResources -> Txtr -> System World Entity
hardWall w (r,s,g,a) sfx txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) = do
  newEntity ((Wall,w)
            , Position (V2 r s)
            , Angle ( if g > a then 2*pi - (g + a) else (g + a) )
            , Velocity 0
            , box (V2 r s) (x / 2) (y / 2)
            , sfx
            , txtr 
            )

makeFloorWallBox :: Txtr -> V2 CDouble -> [SFXResources] -> System World ()
makeFloorWallBox flrTexture (V2 x y) sfxs = do
  let spots = [-1000,1000]
  flip mapM_ spots $ \j -> do
    floorWall (x + j, y - 500) (head $ sfxs) (Angle 0) flrTexture
  flip mapM_ spots $ \j -> do
    floorWall (x + j, y + 500) (head $ sfxs) (Angle 0) flrTexture
  flip mapM_ spots $ \j -> do
    floorWall (x - 500, y + j) (head $ sfxs) (Angle $ pi/2) flrTexture
  flip mapM_ spots $ \j -> do
    floorWall (x + 500, y + j) (head $ sfxs) (Angle $ pi/2) flrTexture