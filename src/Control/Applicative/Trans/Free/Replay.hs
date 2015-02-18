{-# LANGUAGE RankNTypes #-}
module Control.Applicative.Trans.Free.Replay where

import Control.Replay.Class

import Control.Applicative
import Control.Applicative.Trans.Free

import Data.Functor.Coproduct

-- | Replay computation given log structure.
replayApT :: (Replay f g, Applicative m)
          => ApT f m a                -- ^ The computation to replay.
          -> ApT g m b                -- ^ The log.
          -> ApT (Coproduct f g) m a  -- ^ Replayed computation with @f@s replaced by @g@s where log matches.
replayApT (ApT m) (ApT n) = ApT (liftA2 replayApF m n)

-- | Replay @ApF@ computation given log structure.
replayApF :: (Replay f g, Applicative m)
          => ApF f m a                -- ^ The computation to replay.
          -> ApF g m b                -- ^ The log.
          -> ApF (Coproduct f g) m a  -- ^ Replay computation with @f@s replaced by @g@s where log matches.
replayApF (Ap x f) (Ap y g)
  | Just z <- replay (\a b -> Just a) x y
  = right z `Ap` replayApT f g
replayApF f _ = hoistApF left f

-- | Run @ApT f g a@ computation and record actions.
recordApT :: (Functor g, Applicative m)
          => (forall x. f x -> m (h x))   -- ^ How to run and record each particular action.
          -> (forall x. g (m x) -> m x)   -- ^ @g . m ~ m@ natural transformation.
          -> ApT f g a                    -- ^ Computation to record.
          -> ApT h m a                    -- ^ The computation log wrapped in a @m@ computation.
recordApT f g (ApT k) = ApT (g (recordApF f g <$> k))

-- | Run @ApF f g a@ computation and record actions.
recordApF :: (Functor g, Applicative m)
          => (forall x. f x -> m (h x))   -- ^ How to run and record each particular action.
          -> (forall x. g (m x) -> m x)   -- ^ @g . m ~ m@ natural transformation.
          -> ApF f g a                    -- ^ Computation to record.
          -> m (ApF h m a)                -- ^ The computation log wrapped in a @m@ computation.
recordApF _ _ (Pure x) = pure (pure x)
recordApF f g (Ap x y) = Ap <$> f x <*> pure (recordApT f g y)

