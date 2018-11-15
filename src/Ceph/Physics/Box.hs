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

randomDonutBox :: Int -> Float -> Float -> IO [Float]
randomDonutBox l r s = (fmap.fmap) (\a ->  signum a * r + a) $ replicateM l $ randomRIO (negate s,s)
  
boxBound :: System World ()
boxBound = do
  view@(Camera cam scale) <- get global :: System World Camera

  --we can use the current camera's position (or the player's pos)
  --to limit moving things to the current view point
  --(inScopeMoving, outMoving) <- partition (\(b, _, w) ->  (aabb b (Box (cam, 500, 500)))) <$> (getAll :: System World [(Box, Entity, Actor)])
  --when (length outMoving > 50) $ mapM_ (\(_,e,_) -> e `destroy` ( Proxy :: Proxy Box) ) outMoving
  (inScopeWalls, outWalls) <- partition (\(b, _, _) -> aabb b (Box (cam, 1000, 1000))) <$> (getAll :: System World [(Box, Entity, Actor)])
  
    
  cmapM_ $ \case
    --(b, e, Wall) -> return ()
    (b, e, Player1) -> wallBounceA e b inScopeWalls
    (b, e, Enemy1) -> wallBounceA e b inScopeWalls
    (b, e, Projectile) -> when (aabb b (Box (cam, 1000, 1000))) $ wallBounceA e b inScopeWalls
    _ -> return ()
  
  --mapM_ ( \(b, e, _) -> mapM_ ( wallBounceE e b) inScopeWalls) inScopeMoving
 

  --other ways of performing the box collisions

  --  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  --concurrently $ [ bounce c1 c2 |  c2@(b2,_,_) <- inScopeWalls, c1@(b1,_,_) <- inScopeMovingObjs, aabb b1 b2]

  --cfoldM ( \_ ( b@(Box (c, w, h)), _ :: Not Wall, e) -> cmapM_ $ wallBounceE e b) ()
  where
    wallBounceA :: Entity -> Box -> [(Box, Entity, Actor)] -> System World ()
    wallBounceA _ _ [] = return ()
    wallBounceA otherEnt
      b@(Box (V2 ax ay, w1, h1))
      ((wb@(Box (V2 bx by, w2, h2)), _, a):rest) = do
      let edgeMeasures = [ ax - w1 - (bx + w2)
                           , bx - w2 - (ax + w1)
                           , ay - h1 - (by + h2)
                           , by - h2 - (ay + h1) ]
          wallTouched = not $ any (>=0) edgeMeasures
          collideProc = do
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
      if a /= Wall || not wallTouched then wallBounceA otherEnt b rest else do
        p <- get otherEnt
        unless ( p `elem` [Carry,Plant,Sing] ) $ do 
          isProj <- exists otherEnt (Proxy :: Proxy Projectile)
          isEnm <- exists otherEnt (Proxy :: Proxy Enemy1)
          otherEnt `set` Sing
          if isProj then get otherEnt >>= \case
            Bullet -> otherEnt `set` ( Position $ pure 2e7 ) 
            _ -> return () -- collideProc
            
            else if isEnm then return () else collideProc
