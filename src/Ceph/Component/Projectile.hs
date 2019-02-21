{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Component.Projectile where

import Ceph.Components
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Scene
import Ceph.Jams

import Apecs
import Euterpea
import Graphics.Gloss.Interface.IO.Game
import Linear
import qualified SDL.Mixer as M

newArrow :: Picture -> (Music Pitch, M.Chunk)
  -> System World Entity
newArrow p (am,cm) = 
   newEntity ( Position 2e7
             , Velocity 0 
             , Angle 0
             , NoBehavior
             , ( Projectile
               , Arrow
               , BodyPicture $ Scale 0.4 0.4 p
               , Box (2e7, 1, 0.7)
               , SFXResources [] [cm]
               , Song am
               )
             )


newBullet :: [Picture] -> (Music Pitch, M.Chunk)
  -> System World Entity
newBullet ps (am,cm) =
  newEntity ( Position 2e7
            , Velocity 0 
            , Angle 0
            , Seek
            , ( Bullet, Projectile )
            , ( Box (2e7, 0.3, 0.3)
              , Song am
              , Sprite (fmap ( Scale 0.1 0.1 ) ps)
              , SFXResources [] [cm]
              , BodyPicture $ Pictures $ fmap ( Scale 0.1 0.1 ) ps
              )
            )

removeProjectile :: (Projectile, Position, Box, Entity) -> System World ()
removeProjectile  (_, Position p, Box pBox, e) = e `destroy` (Proxy :: Proxy Box)
{--do
  cmap $ \(Box otherBox, Velocity v, Position p2) -> if aabb (Box otherBox) (Box pBox) then  Velocity ( v + (0.5 * normalize (p2 - p)) ) else Velocity v
  e `set` Position (pure 20000)
  --} 
animateProj :: (Box,Velocity,BodyPicture, Sprite, Projectile) -> (BodyPicture, Sprite)
animateProj (_, _, bp, r, Arrow) = (bp,r)
animateProj (_, v, pics, Sprite (p:ps), Bullet) = ((addTrailPics p v pics), Sprite ( ps ++ [ p ]))

shootBullet :: Target -> Position -> Velocity -> Charge -> System World ()
shootBullet (Target at) (Position from) (Velocity v_init) (Charge c _) = conceIf isInScopeBullet updateMotion
  where
  isInScopeBullet (ba, bbox) = ba == Bullet && not (aabb (box at 1000 600) bbox)
  updateMotion ( Position _, Velocity _, Angle _) =
    ( Position from
    , Velocity $ v_init + (pure c) * normalize (at - from)
    , Angle 0
    , Seek
    )
    
--loop thru arrows and find one sufficiently far away and re shoot
shootArrow :: Target -> Position -> Velocity -> Charge -> System World ()
shootArrow (Target at) (Position from) (Velocity v_init) (Charge c _) = conceIf isInScopeArrow updateMotion where
  isInScopeArrow (ba,bbox) = ba == Arrow && not (aabb (box from 1000 600) bbox)
  updateMotion (Position _, Velocity _, Angle _) = 
    ( Position from
    , Velocity $ v_init + (pure c) * normalize (at - from)
    , Angle 0
    )
  