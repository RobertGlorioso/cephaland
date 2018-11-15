{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Component.Projectile where

import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Scene

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
animateProj :: (Box,Velocity,BodyPicture, Resources, Projectile) -> (BodyPicture, Resources)
animateProj (_,_,bp, r, Arrow) = (bp,r)
animateProj (_,v,pics, Resources (p:ps) s, Bullet) = ((addTrailPics p v pics), Resources ( ps ++ [ p ]) s)

shootBullet :: Target -> Position -> Velocity -> Charge -> System World ()
shootBullet (Target at) (Position from) (Velocity v_init) (Charge c _) = locateBullet from =<< getAll
  where
  locateBullet _ [] = return ()
  locateBullet from
    ((ba, rcs@(Resources bullets m), bbox, e):rest) =
    if ba == Arrow || aabb (box from 1000 600) bbox 
    then locateBullet from rest
    else e `set`
         ( Position from
         , Velocity $ v_init + (pure c) * normalize (at - from)
         , Angle 0
         , Seek
         , ( Bullet
           , BodyPicture $ Pictures [ head bullets ]
           , rcs
           , Box (from, 0.02, 0.02)
           )
         )
    
--change to queue of arrows??
--loop thru arrows and find one sufficiently far away and re shoot
shootArrow :: Target -> Position -> Velocity -> Charge -> System World ()
shootArrow (Target at) (Position from) (Velocity v_init) (Charge c _) = locateArrow from =<< getAll
  where
  locateArrow _ [] = return ()
  locateArrow from
    ((ba, rcs@(Resources arrows m), bbox, e):rest) =
    if ba == Bullet || aabb (box from 1000 600) bbox 
    then locateArrow from rest
    else e `set`
         ( Position from
         , Velocity $ v_init + (pure c) * normalize (at - from)
         , Angle 0
         , BodyPicture $ Pictures [ head arrows ]
         , rcs
         )
    
