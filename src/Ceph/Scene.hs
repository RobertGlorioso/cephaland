module Ceph.Scene where

import Ceph.Physics.Box
import Ceph.Components
import Ceph.Scene.HUD
import Ceph.Scene.Camera

import Apecs
import Graphics.Gloss

render :: GameOpts -> World -> IO Picture
render (GameOpts g) w = runWith w $ do

        cmapM_ cameraFollowPlayer
        
        view@(Camera cam scale) <- get global :: System World Camera

        movableEnts <- return . filter (\((b, _, _, _)) -> aabb b (Box (cam, 80, 80))) =<< (getAll :: System World [(Box, Position, Angle, BodyPicture)])

        pics <- mapM entsToPics movableEnts
        
        newWorld <- return . applyView view . mconcat $ pics
        
        --add debuggers here
        cmap $ \(Target t) -> (Target t, Debug $ show t)
        cmap $ \(Player, Position t) -> (Player, Debug $ show t)
        
        --make the scene by combining the HUD and the current world
        hud <- hudPic
        
        return $ Pictures $ [newWorld] ++ hud -- , Translate 150 150 $ Scale 0.1 0.1 newWorld] ++ hud
                           

