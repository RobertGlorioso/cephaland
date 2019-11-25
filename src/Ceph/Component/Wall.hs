{-# LANGUAGE ViewPatterns #-}

module Ceph.Component.Wall where
import Ceph.Components
import Ceph.Physics.Box
import System.Random
import qualified SDL as S
import Apecs
import Linear

hardWall (r,s,g,a) (d,o) txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> S.V2 x y))) = do
  ran <- liftIO $ newStdGen
  let ranColor = (\(r:g:b:a:_) -> S.V4 r g b a) $ randomRs (0,255) ran
  newEntity ((Wall,Wall1)
            , Position (V2 r s)
            , Angle ( if g > a then 2*pi - (g + a) else (g + a) )
            , Velocity 0
            , box (fromIntegral . round <$> V2 r s) (x / 2) (y / 2)
            , SFXResources d o []
            , txtr 
            , SpriteColor ranColor
            ) 