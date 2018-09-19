{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ceph.Components where

import Ceph.Util

import Apecs
import Apecs.Util
import Graphics.Gloss
import Numeric.Hamilton
import Euterpea
import Linear
import Data.Semigroup
import qualified SDL.Mixer as M

data GameOpts = GameOpts { debugOn :: Bool }

newtype Debug = Debug String deriving (Show)
instance Component Debug where
  type Storage Debug = Map Debug

data Phases = PHS (Phase 1) | PHS2 (Phase 2)
instance Component Phases where
  type Storage Phases = Map Phases

data Ghost = Pend  (V2 Float) | OneBody  (V2 Float) | TwoBody  (V2 Float) deriving Eq
instance Component Ghost where
  type Storage Ghost = Map Ghost

data Player = Player 
instance Component Player where
  type Storage Player = Unique Player
  
data Attacking = Attacking
instance Component Attacking where
  type Storage Attacking = Unique Attacking

data Charge = Charge Float
instance Component Charge where
  type Storage Charge = Unique Charge

data Charging = Charging
instance Component Charging where
  type Storage Charging = Unique Charging

data Target = Target (V2 Float)
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

data Behavior = Seek | Sing | Attack | Carry | Defend | Heal | Plant | NoBehavior deriving Eq
instance Component Behavior where
  type Storage Behavior = Map Behavior

data History = History [V2 Float]
instance Component History where
  type Storage History = Map History

data Health = Health Float
instance Component Health where
  type Storage Health = Map Health
  
data Enemy = Enemy
instance Component Enemy where
  type Storage Enemy = Map Enemy

data Resources = Resources { sprites :: [Picture], soundEffects :: [M.Chunk] }
instance Component Resources where
  type Storage Resources = Map Resources

data Box = Box (V2 Float, Float, Float) deriving (Show)
instance Component Box where
  type Storage Box = Map Box
  
data Sword = Sword
instance Component Sword where
  type Storage Sword = Unique Sword

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = (Map Position) -- Cache 100 (Map Position)

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Cache 100 (Map Velocity)

newtype Gravity = Gravity (V2 Float) deriving Show
instance Semigroup Gravity where
  ((<>))  (Gravity a) (Gravity b) = Gravity $ a + b 
instance Monoid Gravity where
  mempty = Gravity 0
instance Component Gravity where type Storage Gravity = Global Gravity

newtype Angle = Angle Float deriving Show
instance Component Angle where type Storage Angle = Map Angle

newtype BodyPicture = BodyPicture Picture

instance Component BodyPicture where
  type Storage BodyPicture = Map BodyPicture

data Camera = Camera
  { gvOffsext :: V2 Float
  , gvScale  :: Float
  }

instance Monoid Camera where
  mempty = Camera 0 1

instance Semigroup Camera where
  ((<>)) (Camera a _) (Camera b c) = Camera ((a + b) / 2) c
  
instance Component Camera where
  type Storage Camera = Global Camera

newtype Song = Song (Music Pitch) deriving Show
instance Component Song where type Storage Song = Map Song

makeWorld "World" [''Camera, ''BodyPicture, ''Player, ''Position, ''Velocity, ''Gravity, ''Angle, ''Target, ''Attacking, ''Charging, ''Charge, ''Dash, ''Projectile, ''ProjCount, ''Sword, ''Enemy, ''Vitality, ''Health, ''Box, ''Resources, ''Wall, ''Ghost, ''Debug, ''Phases, ''Behavior, ''Song]
