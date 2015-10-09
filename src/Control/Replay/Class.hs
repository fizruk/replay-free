{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Control.Replay.Class where

import Control.Applicative

import GHC.Generics

-- | Capturing the notion that @g@ encodes all necessary information to replay @f@ action.
class Replay f g where
  replay :: Alternative m => (a -> b -> m c) -> f a -> g b -> m (g c)
  default replay :: (Alternative m, Generic1 f, Generic1 g, GReplay (Rep1 f) (Rep1 g)) => (a -> b -> m c) -> f a -> g b -> m (g c)
  replay f x y = to1 <$> greplay f (from1 x) (from1 y)

instance Replay ((->) a) ((,) a) where
  replay f g (x, y) = (,) x <$> f (g x) y

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

instance Eq a => GReplay (K1 i a) (K1 i' a) where
  greplay _ (K1 x) (K1 y)
    | x == y    = pure (K1 x)
    | otherwise = empty

instance GReplay f g => GReplay (M1 i t f) (M1 i' t' g) where
  greplay f (M1 x) (M1 y) = M1 <$> greplay f x y

