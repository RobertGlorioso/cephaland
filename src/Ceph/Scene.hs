{-# LANGUAGE ViewPatterns #-}

module Ceph.Scene where

import Ceph.Physics.Box
import Ceph.Components
import Ceph.Scene.HUD
import Ceph.Scene.Camera

import Apecs
import Graphics.Gloss
import Linear

addTrailPics :: Picture -> Velocity -> BodyPicture -> BodyPicture
addTrailPics p (Velocity (V2 x y)) (BodyPicture (Pictures ps)) =
  BodyPicture $ Pictures (p : (take 20 $ flip fmap ps $ \case
    (Translate x0 y0 p0) -> (Translate (x0-x) (y0-y) p0)
    --p0 -> p0
    ))

render :: GameOpts -> World -> IO Picture
render (GameOpts g) w = runWith w $ do

        cmapM_ cameraFollowPlayer
        
        view@(Camera cam scale) <- get global :: System World Camera

        movableEnts <- return . filter (\((b, _, _, _)) -> aabb b (Box (cam, 680, 680))) =<< (getAll :: System World [(Box, Position, Angle, BodyPicture)])

        pics <- mapM entsToPics movableEnts
        
        newWorld <- return . applyView view . mconcat $ pics
        
        --add debuggers here
        cmap $ \(Target t) -> (Target t, Debug $ show t)
        cmap $ \(Player1, Position t) -> (Player1, Debug $ show t)
        
        --make the scene by combining the HUD and the current world
        hud <- hudPic g
        
        return $ Pictures $ [newWorld] ++ hud -- , Translate 150 150 $ Scale 0.1 0.1 newWorld] ++ hud
                           

