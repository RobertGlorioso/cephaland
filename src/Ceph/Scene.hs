{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Ceph.Scene where

import Ceph.Components
import Ceph.Scene.Camera
import Ceph.Scene.Board
import Ceph.Scene.HUD
import Ceph.Util

import Apecs hiding (($=))
import Linear
import Control.Monad
import Data.Functor.Rep
import SDL hiding (get)
import Foreign.C.Types
import System.Random

{--addTrailPics :: Picture -> Velocity -> BodyPicture -> BodyPicture
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
--}

renderTexture :: Renderer -> Txtr -> Rectangle CInt -> CDouble -> IO ()
renderTexture r (Txtr t size) clip n = copyEx r t (Just size) (Just clip) (180 * n / pi) Nothing (pure False)

renderEnt :: (Box, Angle, Txtr, Actor, Entity) -> Renderer -> System World ()
renderEnt (Box (pos,_,_), Angle theta, pic@(Txtr _ (Rectangle _ size)), _, _) r = do
  view <- get global :: System World Camera
  liftIO $ renderTexture r pic (Rectangle (P $ round <$> applyView view (Position pos) size) size) theta

render :: World -> IO ()
render w = runWith  w $ do
  cmapM_ cameraFollowPlayer
  (SDLRenderer r, sc :: SCoord, cam :: Camera) <- get global
  rendererDrawColor r $= V4 190 100 19 255
  fillMBoardBoxes r cam
  cmapM_ $ \case
    (In,b) -> renderEnt b r
    _ -> return ()
  renderHUD r
  rendererDrawColor r $= (V4 12 12 24 255 ) 
