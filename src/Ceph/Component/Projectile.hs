{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Ceph.Component.Projectile where

import Ceph.Components
import Ceph.Component.Weapon
import Ceph.Util
import Ceph.Physics.Box
import Apecs
import Linear
import qualified SDL as S

newArrow :: Txtr -> SFXResources -> System World Entity
newArrow txtr s = 
   newEntity ( Position 2e7
             , Velocity 0 
             , Angle 0
             , NoBehavior
             , ( Projectile
               , Arrow
               , txtr
               , Box (2e7, 0.1, 0.07)
               , s
               )
             )

newSquall :: Txtr -> SFXResources -> System World Entity
newSquall txtr@(Txtr _ (S.Rectangle _ (fmap (fromIntegral) -> V2 x y))) s = do
  newEntity ((Projectile,Squall)
            , (Position (pure 50)
              , Velocity (pure 0)
              , box (pure 50) (x / 2) (y / 2)
              , Angle 0
              , AngularMomentum 0.5
            )
            , txtr
            , s
            , (Gravity $ V2 0 0.01
            , NoBehavior) )

newBullet :: Txtr -> SFXResources -> System World Entity
newBullet txtr s =
  newEntity ( Position 2e7
            , Velocity 0 
            , Angle 0
            , Seek
            , ( Bullet, Projectile )
            , ( Box (2e7, 1, 1)
              , s
              , txtr
              )
            )
  
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
    chns <- cfoldM (\es (Chain,ent) -> return $ ent:es) [] :: System World [Entity]
    cfoldM (\_ (Player1,pe) -> chains (pe:chns) e) ()
    return ()
  updateMotion _ = return ()

