{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Ceph.Scene where

import Ceph.Physics.Box
import Ceph.Components
import Ceph.Scene.HUD
import Ceph.Scene.Camera
import Ceph.Util
import Ceph.Scene.Board

import Apecs
import Graphics.Gloss
import Linear
import qualified SDL as S
import Foreign.C.Types

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


renderTexture :: S.Renderer -> Txtr -> S.Rectangle CInt -> CDouble -> IO ()
renderTexture r (Txtr t size) clip angle = S.copyEx r t (Just size) (Just clip) angle Nothing (pure False)

renderEnt :: (Position, Angle, BodyPicture) -> System World ()
renderEnt (s, Angle theta, BodyPicture pic@(Txtr _ (S.Rectangle _ size))) = do
  SDLRenderer r <- get global
  view <- get global :: System World Camera
  liftIO $ renderTexture r pic (S.Rectangle (S.P $ round <$> applyView view s) size) theta
  where applyView :: Camera -> Position -> V2 CDouble
        applyView (Camera cam scale) (Position p) = p - cam  / pure scale  + S.V2 380 260

render :: World -> IO ()
render w = runWith  w $ do
  cmapM_ cameraFollowPlayer
  SDLRenderer r <- get global
  --cmap nextFrame
  S.rendererDrawColor r S.$= S.V4 190 10 100 255
  get global >>= (\(a,b) -> soundBoard a b) >>=
     mapM (\case
            (False,Position (V2 x y)) -> S.fillRect r (Just (S.Rectangle (S.P $ round <$> S.V2 x y) (S.V2 15 15)))
            (True,Position (V2 x y)) -> S.fillRect r (Just (S.Rectangle (S.P $ round <$> S.V2 x y) (S.V2 10 10)))
     ) 
  S.rendererDrawColor r S.$= S.V4 190 190 190 255
  cmapM_ $ \case
    (In,b@( _, _, _)) -> renderEnt b
    _ -> return ()
            

