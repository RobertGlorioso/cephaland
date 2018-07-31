{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ceph.Components where

import Apecs
import Apecs.Util
import Graphics.Gloss
import Linear
--import qualified SDL.Mixer as M

data GameOpts = GameOpts { debugOn :: Bool }

newtype Debug = Debug String deriving (Show)
instance Component Debug where
  type Storage Debug = Map Debug

data Player = Player 
instance Component Player where
  type Storage Player = Unique Player
  
data Attacking = Attacking
instance Component Attacking where
  type Storage Attacking = Unique Attacking

data Charge = Charge Double
instance Component Charge where
  type Storage Charge = Unique Charge

data Charging = Charging
instance Component Charging where
  type Storage Charging = Unique Charging

data Target = Target (V2 Double)
instance Component Target where
  type Storage Target = Unique Target

data Dash = Dash Float
instance Component Dash where
  type Storage Dash = Unique Dash

data Projectile = Projectile
instance Component Projectile where
  type Storage Projectile = Map Projectile

data Wall = Wall
instance Component Wall where
  type Storage Wall = Map Wall

data ProjCount = ProjCount Int deriving Show
instance Component ProjCount where
  type Storage ProjCount = Map ProjCount

data Vitality = Dead | Alive
instance Component Vitality where
  type Storage Vitality = Map Vitality

data Behavior = Seek | Fire | Defend | Heal
instance Component Behavior where
  type Storage Behavior = Map Behavior

data History = History [V2 Double]
instance Component History where
  type Storage History = Map History
  
data Enemy = Enemy
instance Component Enemy where
  type Storage Enemy = Map Enemy

data Resources = Resources { sprites :: [Picture], soundEffects :: [String] } -- [M.Chunk] }
instance Component Resources where
  type Storage Resources = Map Resources

data Box = Box (V2 Double, Double, Double) deriving (Show)
instance Component Box where
  type Storage Box = Map Box
  
data Sword = Sword
instance Component Sword where
  type Storage Sword = Unique Sword
  
data Body = DynamicBody | KinematicBody | StaticBody deriving (Eq, Ord, Enum)
instance Component Body where type Storage Body = Map Body


newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Cache 100 (Map Position)

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where type Storage Velocity = Cache 100 (Map Velocity)

newtype Gravity = Gravity (V2 Double) deriving Show
instance Semigroup Gravity where
  ((<>))  (Gravity a) (Gravity b) = Gravity $ a + b 
instance Monoid Gravity where
  mempty = Gravity 0
instance Component Gravity where type Storage Gravity = Global Gravity

newtype Angle = Angle Double deriving Show
instance Component Angle where type Storage Angle = Map Angle

newtype BodyPicture = BodyPicture Picture -- deriving Monoid

instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data Camera = Camera
  { gvOffset :: V2 Double
  , gvScale  :: Double
  }

instance Semigroup Camera where
  ((<>)) (Camera a _) (Camera b c) = Camera (a + b) c
instance Monoid Camera where mempty = Camera 0 1
instance Component Camera where
  type Storage Camera = Global Camera

makeWorld "World" [''Camera, ''Body, ''BodyPicture, ''Player, ''Position, ''Velocity, ''Gravity, ''Angle, ''Target, ''Attacking, ''Charging, ''Charge, ''Dash, ''Projectile, ''ProjCount, ''Sword, ''Enemy, ''Vitality, ''Box, ''Resources, ''Wall, ''Debug] 
