{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Ceph.TypeStuff where

import Fcf hiding (Head)
import GHC.TypeLits hiding (type (*),Nat)
import Unsafe.Coerce
import Data.Kind
import Data.Proxy
import Apecs
import Control.Monad.Reader hiding (forM_)
import Control.Monad
import qualified Data.Vector as V

data ListToMaybe a = ListToMaybe [a]

class Evalu8 l t | l -> t where
  evalu8 :: l -> t

instance Evalu8 (ListToMaybe a) (Maybe a) where
  evalu8 (ListToMaybe []) = Nothing
  evalu8 (ListToMaybe (a:as) ) = Just a

data TypeListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (TypeListToMaybe '[]) = 'Nothing
type instance Eval (TypeListToMaybe (a ': as) ) = 'Just a

data TypeFoldr :: (a -> b -> Exp b) -> b -> [a] -> Exp (b)
type instance Eval (TypeFoldr f b '[] ) = b
type instance Eval (TypeFoldr f b ( a ': as ) ) = Eval (f a ( Eval ( TypeFoldr f b as )))

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


