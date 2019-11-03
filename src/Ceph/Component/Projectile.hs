{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ceph.Component.Projectile where

import Ceph.Components
import Ceph.Component.Weapon
import Ceph.Util
import Ceph.Physics.Box
import Ceph.Scene

import Apecs
import Linear
import qualified SDL.Mixer as M

newArrow :: Txtr -> (M.Chunk, M.Chunk)
  -> System World Entity
newArrow txtr (am,cm) = 
   newEntity ( Position 2e7
             , Velocity 0 
             , Angle 0
             , NoBehavior
             , ( Projectile
               , Arrow
               , txtr
               , Box (2e7, 0.1, 0.07)
               , SFXResources [am,cm] []
               )
             )

newBullet :: Txtr -> (M.Chunk, M.Chunk) -> System World Entity
newBullet txtr (am,cm) =
  newEntity ( Position 2e7
            , Velocity 0 
            , Angle 0
            , Seek
            , ( Bullet, Projectile )
            , ( Box (2e7, 1, 1)
              , SFXResources [am,cm] []
              , txtr
              )
            )

removeProjectile :: (Projectile, Position, Box, Entity) -> System World ()
removeProjectile  (_, Position p, Box pBox, e) = e `destroy` (Proxy :: Proxy Box)
{--do
  cmap $ \(Box otherBox, Velocity v, Position p2) -> if aabb (Box otherBox) (Box pBox) then  Velocity ( v + (0.5 * normalize (p2 - p)) ) else Velocity v
  e `set` Position (pure 20000)
  --}
  
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

shootChains :: Target -> Position -> Velocity -> Charge -> System World ()
shootChains (Target at) (Position from) (Velocity v_init) (Charge c _) = conceIfM_ isInScopeArrow updateMotion where
  isInScopeArrow (ba,bbox) = ba == Arrow && not (aabb (box from 1000 600) bbox)
  updateMotion (Arrow,e) = do
    e `set` ( Position from
            , Velocity $ v_init + (pure c) * normalize (at - from)
            , Angle 0
            )
    chns <- cfoldM (\es (Chain,e) -> return $ e:es) [] :: System World [Entity]
    --[pl] <- cfoldM (\pls (Player1,e) -> return $ e:pls) [] :: System World [Entity]
    cfoldM (\pls (Player1,pe) -> chains (pe:chns) e) ()
    --chains (pl:chns) e
    return ()

