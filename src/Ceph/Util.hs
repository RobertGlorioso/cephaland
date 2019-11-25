{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE Strict #-}
module Ceph.Util where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader hiding (forM_)
--import Data.Proxy
--import Fcf
--import GHC.TypeLits hiding (type (*),Nat)
--import Unsafe.Coerce
--import Data.Kind
import Linear
import Apecs
import Apecs.Core
import qualified Data.Vector.Unboxed as U


v2ToRad :: (RealFloat a, Ord a) => V2 a -> a
v2ToRad (V2 m n) = case compare m 0 of
  LT -> atan ( n / m ) + pi
  GT -> atan ( n / m ) 
  EQ -> (-pi)/2 * (signum n)


poww :: (Eq t, Num t) => t1 -> t -> (t1 -> t1) -> [t1]
poww a 0 _ = [a]
poww a n f = f a : poww (f a) (n - 1) f

kpow :: (Monad m, Num t, Eq t) => t -> (b -> m b) -> b -> m b
kpow 0 _ = return
kpow n f = f <=< kpow (n-1) f

fpow :: t2 -> Int -> (t2 -> t) -> (t2 -> t2) -> [t]
fpow a 0 _ _ = []
fpow a n f g = f a : fpow (g a) (n-1) f g

{-# INLINE cmapIf_ #-}
cmapIf_ :: forall w m cp cx cy.
  ( Get w m cx
  , Get w m cp
  , Members w m cx
  , Set w m cy )
  => (cp -> Bool)
  -> (cx -> cy)
  -> SystemT w m ()
cmapIf_ cond f = do
  sp :: Storage cp <- getStore
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  lift $ do
    sl <- explMembers (sx,sp)
    slf <- filter (\(_,p) -> cond p ) <$>  forM (U.toList sl)
      (\e -> do
          p <- explGet sp e
          return (e,p)
      )
  
    forM_ slf $ \(e,_) -> do 
      x <- explGet sx e
      explSet sy e (f x)

{-# INLINE cmapIfM_ #-}
cmapIfM_ :: forall w m cp cx .
  ( Get w m cx
  , Get w m cp
  , Members w m cx
  , MonadIO m
  )
  => (cp -> Bool)
  -> (cx -> SystemT w m ())
  -> SystemT w m ()
cmapIfM_ cond f = do
  sp :: Storage cp <- getStore
  sx :: Storage cx <- getStore
  sl <- lift $ explMembers (sx,sp)
  slf <- filter (\(_,p) -> cond p ) <$>  forM (U.toList sl)
    (\e -> do
        p <- lift $ explGet sp e
        return (e,p)
    )
  forM_ slf $ \(e,_) -> do 
      x <- lift $ explGet sx e
      f x

{-# INLINE conceM_ #-} 
conceM_ :: forall w m cx. (Alternative m,Get w m cx, Members w m cx) => (cx -> SystemT w m ()) -> SystemT w m ()
conceM_ f =  do
  sx :: Storage cx <- getStore
  x <- lift $ do
    sl <- explMembers sx
    explGet sx . U.head $ sl 
  f x
  
{-# INLINE conceIf #-} 
conceIf :: forall w m cx cp cy.
  (Get w m cx
  , Get w m cp
  , Members w m cx
  , Set w m cy
  , MonadIO m)
  => (cp -> Bool)
  -> (cx -> cy)
  -> SystemT w m ()
conceIf cond f = do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sy :: Storage cy <- getStore
  lift $ do
    sl <- explMembers (sx,sp)
    es <- U.filterM (\n -> (explGet sp n) >>= return . cond) sl
    if (U.null es) then return () else do
      e <- return $ U.head es
      explGet sx e >>=  explSet sy e . f

{-# INLINE conceIfM #-} 
conceIfM :: forall w m cx cp cy.
  (Get w m cx
  , Get w m cp
  , Set w m cy
  , Members w m cx
  , Alternative m
  , MonadIO m)
  => (cp -> Bool)
  -> (cx -> SystemT w m cy)
  -> SystemT w m cy
conceIfM cond f = do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sl <- lift $ explMembers (sx,sp)
  es <- lift $ U.filterM (\n -> (explGet sp n) >>= return . cond) sl
  e <- return $ U.head es
  lift (explGet sx e) >>= f

{-# INLINE conceIfM_ #-} 
conceIfM_ :: forall w m cx cp.
  (Get w m cx
  , Get w m cp
  , Members w m cx
  , Alternative m
  , MonadIO m)
  => (cp -> Bool)
  -> (cx -> SystemT w m ())
  -> SystemT w m ()
conceIfM_ cond f = do
  sx :: Storage cx <- getStore
  sp :: Storage cp <- getStore
  sl <- lift $ explMembers (sx,sp)
  U.foldM'_ (\b e -> do
               if b then do
                 p <- lift (explGet sp e)
                 if cond p then do
                   lift (explGet sx e) >>= f
                   return False
                   else return True
                 else return False
           )
    True sl
  return ()


-- | Pseudocomponent that when written to, actually writes @c@ to its entity argument.
--   Can be used to write to other entities in a 'cmap'.
data Redirect c = Redirect Entity c deriving (Eq, Show)
instance Component c => Component (Redirect c) where
  type Storage (Redirect c) = RedirectStore (Storage c)

newtype RedirectStore s = RedirectStore s
type instance Elem (RedirectStore s) = Redirect (Elem s)

instance Has w m c => Has w m (Redirect c) where
  getStore = RedirectStore <$> getStore

instance (ExplSet m s) => ExplSet m (RedirectStore s) where
  explSet (RedirectStore s) _ (Redirect (Entity ety) c) = explSet s ety c


-- | Pseudocomponent that can be read like any other component, but will only
--   yield a single member when iterated over. Intended to be used as
--   @cmap $ Head (...) -> ...@
newtype Head c = Head c deriving (Eq, Show)
instance Component c => Component (Head c) where
  type Storage (Head c) = HeadStore (Storage c)

newtype HeadStore s = HeadStore s
type instance Elem (HeadStore s) = Head (Elem s)

instance Has w m c => Has w m (Head c) where
  getStore = HeadStore <$> getStore

instance (ExplGet m s) => ExplGet m (HeadStore s) where
  explExists (HeadStore s) ety = explExists s ety
  explGet (HeadStore s) ety = Head <$> explGet s ety

instance (ExplMembers m s) => ExplMembers m (HeadStore s) where
  explMembers (HeadStore s) = U.take 1 <$> explMembers s

{--
data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) = if i == findElem @t @ts
                             then Just $ unsafeCoerce f
                             else Nothing

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum i f) = (UnsafeOpenSum i (unsafeCoerce f))

supply :: f x -> OpenSum f ts -> OpenSum f (x ': ts)
supply x (UnsafeOpenSum i f) = inj x


decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t


data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol,k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert :: Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key,t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type PFindElem (key :: Symbol) (ts :: [(Symbol,k)]) = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

pfindElem :: forall key ts. KnownNat (PFindElem key ts) => Int
pfindElem = fromIntegral . natVal $ Proxy @(PFindElem key ts)

type LookupType (key :: k) (ts :: [(k,t)]) = FromMaybe Stuck =<< Lookup key ts

gett :: forall key ts f.
       KnownNat (PFindElem key ts)
       => Key key
       -> OpenProduct f ts
       -> f (Eval (LookupType key ts))
gett _ (OpenProduct v) = unAny $ V.unsafeIndex v $ pfindElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a
  
type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol,k)]) = SetIndex (PFindElem key ts) '(key, t) ts

update :: forall key ts t f.
           KnownNat (PFindElem key ts)
        => Key key
        -> f t
        -> OpenProduct f ts
        -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v V.// [(pfindElem @key @ts, Any ft)]

type family All (c :: Type -> Constraint) ( ts :: [Type]) :: Constraint where
  All c  '[] = ()
  All c (t ': ts) = (c t, All c ts)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

data Nat = Z | S Nat

data Natural a where
  Zero :: Natural 'Z
  Succ :: Natural a -> Natural ('S a)

zero = Zero
uno = Succ Zero
dos = Succ uno
tres = Succ dos
quat = Succ tres
cinc = Succ quat

instance Show (Natural a) where
  show Zero = "0"
  show (Succ n) = "1+" ++ show n
  
class IndexType (n :: Nat) (xs :: [Type]) (i :: Type) | n xs -> i where
   fromIndex :: Natural n -> HList xs -> i

instance IndexType 'Z (x ': xs) x where
   fromIndex Zero (x :# _) = x

instance IndexType n xs a => IndexType ('S n) (x ': xs) a where
   fromIndex (Succ n) (_ :# xs) = fromIndex n xs

instance All Show ts => Show (HList ts) where
  show HNil = "'[]"
  show (a :# as) = show a ++ " :# " ++ show as
              
instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs
  
--type instance Elem (HList ts) = HList (Elem ts)

--instance (All (Has w m) ts, Monad m, Component (HList ts))  => Has w m (HList ts) where
--  getStore =  HNil <$> getStore
nuProxy :: Proxy a -> Proxy b
nuProxy _ = Proxy

checkE :: forall w m cx. (Show cx, Get w m cx, Members w m cx, MonadIO m) => Proxy cx -> Entity -> SystemT w m (Maybe cx)
checkE _ e = do
  b <- exists e (Proxy :: Proxy cx)
  if b then (return .Just =<< get e) else (return Nothing)
  --cmap $ ((\(c) -> Debug (show c)) :: cx -> Debug)
  
--}
