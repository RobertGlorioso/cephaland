{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Ceph.Physics.Box where

import Ceph.Components
import Ceph.Jams
import Data.List
import Graphics.Gloss
import System.Random
import Control.Monad
import Apecs
import Linear
import Euterpea
import qualified SDL.Mixer as M

data Edge = LeftEdge | RightEdge | TopEdge | BottomEdge | TopRightCorner | BottomRightCorner | BottomLeftCorner | TopLeftCorner deriving (Eq, Show, Ord)

  
aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) = all (<0)
  [ax - w1 - (bx + w2)
   , bx - w2 - (ax + w1)
   , ay - h1 - (by + h2)
   , by - h2 - (ay + h1) ]

minni a b = snd $ minimumBy (\c d -> compare (fst c) (fst d)) $ zip a b

box (V2 x y) w h = Box ((V2 x y), w, h)

--whileInBox :: Entity -> Box -> System World ()
whileInBox e wb = do
  b <- get e :: System World Box
  liftIO $ print (b, wb)
  if aabb b wb
     then randoBounce e >> whileInBox e wb
     else return () 

randoBounce e = do
  (p1, p2) <- liftIO $ (,) <$> randomRIO (-1e-3,1e-3 :: Float) <*> randomRIO ((-1e-3),1e-3 :: Float)
  Position p <- get e
  Box (_,b1,b2) <- get e
  Velocity v <- get e
  let newV = negate v
  let newP = p + V2 p1 p2 + newV
  e `set` Position newP
  e `set` Box (newP, b1, b2)
  e `set` Velocity newV

boxBound :: System World ()
boxBound = do
  view@(Camera cam scale) <- get global :: System World Camera
  inScopeMoving <- return . filter (\(b,p, _, _) ->  p /= Plant && aabb b (Box (cam, 80, 80))) =<< (getAll :: System World [(Box, Behavior, Entity, Not Wall)])
  
  inScopeWalls <- return . filter (\(b, _, _) -> aabb b (Box (cam, 80, 80))) =<< (getAll :: System World [(Box, Entity, Wall)])
  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  --concurrently $ [ bounce c1 c2 |  c2@(b2,_,_) <- inScopeWalls, c1@(b1,_,_) <- inScopeMovingObjs, aabb b1 b2]
  mapM_ wallBounce inScopeMoving
  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  where
    wallBounce (b,_, e, _) = cmapM_ $ wallBounceE e b
    wallBounceE e b@(Box (V2 ax ay, w1, h1)) (Wall, wb@(Box (V2 bx by, w2, h2)), Resources _ [s]) = do
      let edgeMeasures = [ ax - w1 - (bx + w2)
                         , bx - w2 - (ax + w1)
                         , ay - h1 - (by + h2)
                         , by - h2 - (ay + h1) ]
      isGhost <- exists e (Proxy :: Proxy Ghost)
      let wallTouched = all (<0) edgeMeasures
      if ( not wallTouched && isGhost ) then e `set` NoBehavior else when wallTouched $ do
        --freeChan <- return . (<8) =<< M.getChannels

        let chanPlay i s
              | i <= 7 && i >= 0 = do
                  isP <- M.playing i
                  if not isP then (M.playOn i M.Once s) else chanPlay (i+1) s
              | True = return i

        h <- get e
        when (h /= Sing) $ chanPlay 0 s >> return ()

        --liftIO $ (M.decode bm :: IO M.Chunk)  >>= M.playOn 0 M.Once >> return ()
  
        if (isGhost) then e `set` Sing else do
          Gravity (V2 _ g) <- get global
          (Position p@(V2 p1 p2), Velocity v@(V2 v1 v2)) <- get e
          if (norm v < (-10)*g) then e `set` Plant  else return () 
          --(BodyPicture pic) <- get e' :: System World BodyPicture
          --e' `set` (BodyPicture $ (\(Color _ c) -> (Color green c)) pic)
          let friction = 2
          case minni (abs <$> edgeMeasures) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
          
            TopEdge -> e `set` (Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / friction))
            BottomEdge -> e `set` (Position $ p + V2 v1 (negate v2), Velocity (V2 v1 (negate v2) / friction))
            RightEdge -> e `set` (Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / friction))
            LeftEdge -> e `set` (Position $ p + V2 (negate v1) v2, Velocity (V2 (negate v1) v2 / friction))

    
