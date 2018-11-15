{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict #-}
module Ceph.Util where

import Ceph.Components

import Linear
import Apecs
import Apecs.Core
import Control.Monad
import Data.Vector.Unboxed as U
--import GHC.Prim

v2ToRad :: (Floating a, Ord a) => V2 a -> a
v2ToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m ) + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)

conceM_ :: forall w m cx. (Get w m cx, Members w m cx) => (cx -> SystemT w m ()) -> SystemT w m ()
conceM_ f =  do
  sx :: Storage cx <- getStore
  sl <- lift $ explMembers sx
  x <- lift . explGet sx . U.head $ sl 
  f x

conceIfM_ :: forall w m cx cp. (Get w m cx, Get w m cp, Members w m cx) => (cp -> Bool) -> (cx -> SystemT w m ()) -> SystemT w m ()
conceIfM_ cond f =  do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sl <- lift $ explMembers (sx,sp)
  --e <- return . U.head $ sl
  --rest <- return . U.tail $ sl
 {-- p <- lift $ explGet sp e
  if (cond p)
     
  then lift (explGet sx e) >>= f
  else 
--}
  es <- U.head <$> U.filterM (\e -> lift (explGet sp e ) >>= return . not . cond) sl
  lift (explGet sx es) >>= f
