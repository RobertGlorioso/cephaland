{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Component.Projectile where

import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box

import Apecs
import Graphics.Gloss.Interface.IO.Game
import Linear
import qualified SDL.Mixer as M

removeProjectile :: (Projectile, Position, Box, Entity) -> System World ()
removeProjectile  (_, Position p, Box pBox, e) = e `destroy` (Proxy :: Proxy Box)
{--do
  cmap $ \(Box otherBox, Velocity v, Position p2) -> if aabb (Box otherBox) (Box pBox) then  Velocity ( v + (0.5 * normalize (p2 - p)) ) else Velocity v
  e `set` Position (pure 20000)
  --} 
checkProj :: (BodyPicture, Resources, Projectile) -> (BodyPicture, Resources)
checkProj (bp, r, Arrow) = (bp,r)
checkProj (BodyPicture _, Resources ps s, Bullet) = (BodyPicture (head ps), Resources (tail ps ++ [ head ps ]) s)

shootBullet :: Target -> Position -> Velocity -> Charge -> System World Entity
shootBullet (Target at) (Position from) (Velocity v_init) (Charge c _) = do
  (p, rcs@(Resources bullets m)) :: (Projectile, Resources) <- head .  filter (\(p,_) -> p == Bullet) <$> getAll 
  newEntity ( Position from
            , Velocity $ v_init + (pure c) * normalize (at - from)
            , Angle 0
            , Seek
            , ( Bullet
              , BodyPicture $ head bullets
              , rcs
              , Box (from, 0.02, 0.02)
              )
            )

--change to queue of arrows??
--loop thru arrows and find one sufficiently far away and re shoot
shootArrow :: Target -> Position -> Velocity -> Charge -> System World Entity
shootArrow (Target at) (Position from) (Velocity v_init) (Charge c _) = do
  (_, rcs@(Resources [arw] m)) <- head . filter (\(p,_) -> p == Arrow) <$> getAll 
  newEntity ( Position from
            , Velocity $ v_init + (pure c) * normalize (at - from)
            , Angle 0
            , NoBehavior
            , ( BodyPicture arw
              , rcs
              , Box (from, 0.1, 0.07)
              )
            )
