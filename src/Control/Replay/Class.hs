{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Control.Replay.Class where

import Control.Applicative
import Control.Monad.Free
import Control.Comonad.Cofree

import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product

import GHC.Generics

-- | Capturing the notion that @g@ encodes all necessary information to replay @f@ action.
class Replay f g where
  replay :: Alternative m => (a -> b -> m c) -> f a -> g b -> m (g c)
  default replay :: (Alternative m, Generic1 f, Generic1 g, GReplay (Rep1 f) (Rep1 g)) => (a -> b -> m c) -> f a -> g b -> m (g c)
  replay f x y = to1 <$> greplay f (from1 x) (from1 y)

instance Replay ((->) a) ((,) a) where
  replay f g (x, y) = (,) x <$> f (g x) y

instance Replay Identity Identity
instance Eq m => Replay (Const m) (Const m)
instance Eq m => Replay (Constant m) (Constant m)
instance (Functor f, Functor f', Functor g, Functor g', Replay f g, Replay f' g') => Replay (Compose f f') (Compose g g')
instance (Replay f g, Replay f' g') => Replay (Product f f') (Product g g')
instance (Functor f, Functor g, Replay f g) => Replay (Cofree f) (Cofree g)
instance (Functor f, Functor g, Replay f g) => Replay (Free f) (Free g)

deriving instance Functor f => Generic1 (Free f)
deriving instance Functor f => Generic1 (Cofree f)
deriving instance (Functor f, Functor g) => Generic1 (Compose f g)
deriving instance Generic1 (Product f g)
deriving instance Generic1 (Constant m)

class GReplay f g where
  greplay :: Alternative m => (a -> b -> m c) -> f a -> g b -> m (g c)

instance GReplay V1 V1 where
  greplay _ = error "impossible"

instance GReplay U1 U1 where
  greplay _ U1 U1 = pure U1

instance (GReplay f f', GReplay g g') => GReplay (f :+: g) (f' :+: g') where
  greplay f (L1 x) (L1 y) = L1 <$> greplay f x y
  greplay f (R1 x) (R1 y) = R1 <$> greplay f x y
  greplay _ _ _ = empty

instance (GReplay f f', GReplay g g') => GReplay (f :*: g) (f' :*: g') where
  greplay f (x :*: y) (x' :*: y') = liftA2 (:*:) (greplay f x x') (greplay f y y')

instance (Replay f f', GReplay g g') => GReplay (f :.: g) (f' :.: g') where
  greplay f (Comp1 x) (Comp1 y) = Comp1 <$> replay (greplay f) x y

instance GReplay Par1 Par1 where
  greplay f (Par1 x) (Par1 y) = Par1 <$> f x y

instance Replay f g => GReplay (Rec1 f) (Rec1 g) where
  greplay f (Rec1 x) (Rec1 y) = Rec1 <$> replay f x y

instance Eq a => GReplay (K1 i a) (K1 i a) where
  greplay _ (K1 x) (K1 y)
    | x == y    = pure (K1 x)
    | otherwise = empty

instance GReplay f g => GReplay (M1 i t f) (M1 i t g) where
  greplay f (M1 x) (M1 y) = M1 <$> greplay f x y

