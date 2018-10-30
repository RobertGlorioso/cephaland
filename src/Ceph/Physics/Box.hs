{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ceph.Physics.Box where

import Ceph.Components
import Data.List
import System.Random
import Control.Monad
import Apecs
import Linear
import qualified SDL.Mixer as M

data Edge = LeftEdge | RightEdge | TopEdge | BottomEdge | TopRightCorner | BottomRightCorner | BottomLeftCorner | TopLeftCorner deriving (Eq, Show, Ord)

aabb :: Box -> Box -> Bool
aabb (Box (V2 ax ay,w1,h1)) (Box (V2 bx by,w2,h2)) = all (<0)
  [ax - w1 - (bx + w2)
   , bx - w2 - (ax + w1)
   , ay - h1 - (by + h2)
   , by - h2 - (ay + h1) ]

minni :: Ord a => [a] -> [b] -> (a,b)
minni a b = minimumBy (\c d -> compare (fst c) (fst d)) $ zip a b

box :: V2 Float -> Float -> Float -> Box
box (V2 x y) w h = Box ((V2 x y), w, h)

--whileInBox :: Entity -> Box -> System World ()
whileInBox :: Entity -> Box -> SystemT World IO ()
whileInBox e wb = do
  b <- get e :: System World Box
  liftIO $ print (b, wb)
  if aabb b wb
     then randoBounce e >> whileInBox e wb
     else return () 


--dont think i need this one anymore
randoBounce :: Entity -> System World ()
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

  --we can use the current camera's position (or the player's pos)
  --to limit moving things to the current view point

  (inScopeMoving, out) <- partition (\(b, _, _) ->  (aabb b (Box (cam, 100, 100)))) <$> (getAll :: System World [(Box, Entity, Not Wall)])
  when (length out > 50) $ mapM_ (\(_,e,_) -> e `destroy` ( Proxy :: Proxy Box) ) $ out
  (inScopeWalls, outWalls) <- partition (\(b, _, _) -> aabb b (Box (cam, 500, 500))) <$> (getAll :: System World [(Box, Entity, Wall)])
  when ( length inScopeWalls > 250 ) $ do
    (inScopeWalls, outWalls) <- partition (\(b, _, _) -> aabb b (Box (cam, 100, 100))) <$> (getAll :: System World [(Box, Entity, Wall)])
    mapM_ (\(_,e,_) -> e `destroy` ( Proxy :: Proxy Box) ) $ outWalls
    

  mapM_ (wallBounceO inScopeWalls) inScopeMoving


  --other ways of performing the box collisions

  --
  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  --concurrently $ [ bounce c1 c2 |  c2@(b2,_,_) <- inScopeWalls, c1@(b1,_,_) <- inScopeMovingObjs, aabb b1 b2]

  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  where
    wallBounceO :: [(Box,Entity,Wall)] -> (Box, Entity, Not Wall) -> System World ()
    wallBounceO w (b, e, _) = mapM_ (wallBounceE e b) w
    wallBounceE :: Entity -> Box -> (Box, Entity, Wall) -> System World ()
    wallBounceE otherEnt b@(Box (V2 ax ay, w1, h1)) (wb@(Box (V2 bx by, w2, h2)), _, _) = do
      let edgeMeasures = [ ax - w1 - (bx + w2)
                           , bx - w2 - (ax + w1)
                           , ay - h1 - (by + h2)
                           , by - h2 - (ay + h1) ]
      let wallTouched = all (<0) edgeMeasures
      let collideProc = do
            Gravity (V2 _ g) <- get global
            (Position p@(V2 p1 p2), Velocity v@(V2 v1 v2),b) <- get otherEnt
            --(BodyPicture pic) <- get e' :: System World BodyPicture
            --e' `set` (BodyPicture $ (\(Color _ c) -> (Color green c)) pic)
            let friction = 2
            --liftIO . print $ minni (abs <$> edgeMeasures) [RightEdge, LeftEdge, TopEdge, BottomEdge]
            case minni (abs <$> edgeMeasures) [RightEdge, LeftEdge, TopEdge, BottomEdge] of
          
              (d,TopEdge) -> otherEnt `set` (if norm v < (-10)*g then Plant else b, Position $ p + V2 v1 (abs v2 + d), Velocity (V2 v1 (abs v2) / friction))
              (d,BottomEdge) -> otherEnt `set` (Position $ p + V2 v1 ( negate . abs $ v2 - d), Velocity (V2 v1 (negate . abs $ v2) / friction))
              (d,RightEdge) -> otherEnt `set` (Position $ p + V2 (abs v1 + d) v2, Velocity (V2 (abs v1) v2 / friction))
              (d,LeftEdge) -> otherEnt `set` (Position $ p + V2 ( negate . abs $ v1 - d) v2, Velocity (V2 (negate . abs $ v1) v2 / friction))
      when wallTouched $ do
        
        p <- get otherEnt
        unless ( p `elem` [Carry,Plant,Sing]) $ do 
          isProj <- exists otherEnt (Proxy :: Proxy Projectile)
          --isEnm <- exists otherEnt (Proxy :: Proxy Enemy)
          if isProj then get otherEnt >>= \case
            Bullet -> otherEnt `destroy` ( Proxy :: Proxy Box) 
            Arrow -> otherEnt `set` Sing >> collideProc
            else otherEnt `set` Sing >> collideProc    
