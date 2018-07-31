module Ceph.Scene where

import Ceph.Physics
import Ceph.Components
import Ceph.Scene.HUD
import Ceph.Scene.Camera

import Apecs
import Apecs.Util
import Graphics.Gloss.Geometry.Angle        (radToDeg)
import Graphics.Gloss
import Linear

render (GameOpts g) w = do
      
      movableObjs <- runWith w $ getAll :: IO [(Position, Angle, BodyPicture)]
      
      pics <- mapM (\((Position (V2 x y), Angle theta, BodyPicture pic)) -> return . Translate (realToFrac x) (realToFrac y) . Rotate (negate . radToDeg . realToFrac $ theta) $ pic) movableObjs
      
      runWith w $ do
        
        updateCamera
        
        view <- get global :: System World Camera
        newWorld <- return . applyView view . mconcat $ pics
        
        --add debuggers here
        cmap $ \(Target t) -> (Target t, Debug $ show t)
        cmap $ \(Player, Position t) -> (Player, Debug $ show t)
        
        --make the scene by combining the HUD and the current world
        
        hud <- hudPic
        
        return $ Pictures $ [newWorld] ++ hud
                           

