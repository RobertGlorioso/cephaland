{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Data where

import Apecs
import Apecs.Util
import Apecs.Physics
import Apecs.Physics.Gloss
import Graphics.Gloss
import qualified SDL.Mixer as M

data Player = Player -- A single constructor component for tagging the player
instance Component Player where
  type Storage Player = Unique Player -- Unique contains at most one component

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
  type Storage Resources = Unique Resources

data Box = Box (V2 Double, Double, Double)
instance Component Box where
  type Storage Box = Map Box
  
data Sword = Sword
instance Component Sword where
  type Storage Sword = Unique Sword -- Unique contains at most one component

makeWorld "World" [''Physics, ''Camera, ''BodyPicture, ''Player, ''Target, ''Attacking, ''Dash, ''Projectile, ''ProjCount, ''Sword, ''Enemy, ''Vitality, ''Box, ''Resources, ''Wall] 
