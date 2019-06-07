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

nextFrame :: (Animated,BodyPicture,Sprites) -> (Animated,BodyPicture,Sprites)
nextFrame (Loop,BodyPicture _,Sprites (p:ps)) = (Loop,BodyPicture p, Sprites (ps ++ [p]))
nextFrame (Animate 0,BodyPicture _,Sprites (p:ps)) = (Still, BodyPicture p, Sprites (ps ++ [p]))
nextFrame (Animate n,BodyPicture _,Sprites (p:ps)) = (Animate (n-1),BodyPicture p, Sprites (ps ++ [p]))
nextFrame (Still,b,s) = (Still,b,s)
nextFrame i@(_,_,Sprites []) = i

render :: GameOpts -> World -> IO Picture
render (GameOpts g) w = runWith w $ do
        cmapM_ cameraFollowPlayer
        cmap nextFrame
        view@(Camera cam scale) <- get global :: System World Camera
        movableEnts <- return . filter (\((b, _, _, _)) -> b == In) =<< (cfoldM (\a b -> return (b:a) ) [] :: System World [(Scope, Position, Angle, BodyPicture)])
        pics <- mapM entsToPics movableEnts        
        newWorld <- return . applyView view . mconcat $ pics        
        --make the scene by combining the HUD and the current world
        hud <- hudPic g       
        return $ Pictures $ [newWorld] ++ hud -- , Translate 150 150 $ Scale 0.1 0.1 newWorld] ++ hud
                           

