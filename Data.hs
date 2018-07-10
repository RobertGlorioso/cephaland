{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data where

import Apecs
import Apecs.Util
import Graphics.Gloss
import Linear
import qualified SDL.Mixer as M

data Player = Player -- A single constructor component for tagging the player
instance Component Player where
  type Storage Player = Unique Player
  
data Attacking = Attacking
instance Component Attacking where
  type Storage Attacking = Unique Attacking

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

data ProjCount = ProjCount Int
instance Component ProjCount where
  type Storage ProjCount = Map ProjCount

data Vitality = Dead | Alive
instance Component Vitality where
  type Storage Vitality = Map Vitality

data Behavior = Seek | Fire | Defend | Heal
instance Component Behavior where
  type Storage Behavior = Map Behavior
  
data Enemy = Enemy
instance Component Enemy where
  type Storage Enemy = Map Enemy

data Resources = Resources { sprites :: [Picture], soundEffects :: [M.Chunk] }
instance Component Resources where
  type Storage Resources = Map Resources

data Box = Box (V2 Double, Double, Double) deriving (Show)
instance Component Box where
  type Storage Box = Map Box
  
data Sword = Sword
instance Component Sword where
  type Storage Sword = Unique Sword -- Unique contains at most one component

data Body = DynamicBody | KinematicBody | StaticBody deriving (Eq, Ord, Enum)
instance Component Body where type Storage Body = Map Body

newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Gravity = Gravity (V2 Double) deriving Show
instance Monoid Gravity where mempty = Gravity 0
instance Component Gravity where type Storage Gravity = Global Gravity

newtype Angle = Angle Double deriving Show
instance Component Angle where type Storage Angle = Map Angle

newtype BodyPicture = BodyPicture Picture deriving Monoid

instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data Camera = Camera
  { gvOffset :: V2 Double
  , gvScale  :: Double
  }

instance Monoid Camera where mempty = Camera 0 1
instance Component Camera where
  type Storage Camera = Global Camera

makeWorld "World" [''Camera, ''Body, ''BodyPicture, ''Player, ''Position, ''Velocity, ''Gravity, ''Angle, ''Target, ''Attacking, ''Dash, ''Projectile, ''ProjCount, ''Sword, ''Enemy, ''Vitality, ''Box, ''Resources, ''Wall] 
