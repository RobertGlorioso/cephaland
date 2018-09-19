{-# LANGUAGE TypeFamilies #-}
module Ceph.Util where

import Linear
import Apecs

vToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m ) + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)

