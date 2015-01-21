{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Replay.Class where

import Control.Applicative
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Coproduct
import Data.Functor.Identity
import Data.Functor.Product
import Data.Traversable

-- | Capturing the notion that @g@ encodes all necessary information to replay @f@ action.
--
-- @
-- replay (const k) (restore g) g = replay (const . k) (restore g) g = traverse k g
-- @
class Traversable g => Replay f g where
  replay :: Alternative m => (a -> b -> m c) -> f a -> g b -> m (g c)
  restore :: g a -> f a

instance Replay ((->) a) ((,) a) where
  replay f g (x, y) = (,) x <$> f (g x) y
  restore (_, y) = const y

instance Replay Identity Identity where
  replay f (Identity x) (Identity y) = Identity <$> f x y
  restore = id

instance Eq m => Replay (Const m) (Const m) where
  replay _ (Const x) (Const y)
    | x == y    = pure $ Const x
    | otherwise = empty
  restore = id

instance Eq m => Replay (Constant m) (Constant m) where
  replay _ (Constant x) (Constant y)
    | x == y    = pure $ Constant x
    | otherwise = empty
  restore = id

instance (Functor f, Replay f g, Replay f' g') => Replay (Compose f f') (Compose g g') where
  replay f (Compose a) (Compose b) = Compose <$> replay (replay f) a b
  restore (Compose a) = Compose (restore <$> restore a)

instance (Replay f g, Replay f' g') => Replay (Product f f') (Product g g') where
  replay f (Pair a b) (Pair x y) = Pair <$> replay f a x <*> replay f b y
  restore (Pair a b) = Pair (restore a) (restore b)

instance (Replay f g, Replay f' g') => Replay (Coproduct f f') (Coproduct g g') where
  replay f (Coproduct (Left  a)) (Coproduct (Left  b)) = Coproduct . Left  <$> replay f a b
  replay f (Coproduct (Right a)) (Coproduct (Right b)) = Coproduct . Right <$> replay f a b
  replay _ _ _ = empty
  restore (Coproduct (Left  a)) = Coproduct (Left  (restore a))
  restore (Coproduct (Right b)) = Coproduct (Right (restore b))

instance (Functor f, Replay f g) => Replay (Cofree f) (Cofree g) where
  replay f (a :< as) (b :< bs) = (:<) <$> f a b <*> replay (replay f) as bs
  restore (a :< as) = a :< (restore <$> restore as)

instance (Functor f, Replay f g) => Replay (Free f) (Free g) where
  replay k (Pure x) (Pure y) = Pure <$> k x y
  replay k (Free f) (Free g) = Free <$> replay (replay k) f g
  replay _ _ _ = empty
  restore (Pure x) = Pure x
  restore (Free f) = Free (restore <$> restore f)

