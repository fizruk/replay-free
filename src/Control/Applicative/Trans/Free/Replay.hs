{-# LANGUAGE RankNTypes #-}
module Control.Applicative.Trans.Free.Replay where

import Control.Replay.Class

import Control.Applicative
import qualified Control.Applicative.Trans.Free as FT
import qualified Control.Applicative.Free as F

import Data.Functor.Coproduct

-- | Replay @ApT f m@ computation given @Ap g@ computation log tree.
-- The result is new @ApT (Coproduct f g) m@ computation with @f@s
-- replaced by @g@s where log matches actual computation.
replayApT :: (Replay f g, Functor m)
          => FT.ApT f m a                 -- ^ The computation to replay.
          -> F.Ap g b                     -- ^ The log tree.
          -> FT.ApT (Coproduct f g) m a
replayApT ft r@(F.Pure _) = FT.hoistApT left ft
replayApT (FT.ApT m) r@(F.Ap y g) = FT.ApT (replayApF <$> m)
  where
    replayApF (FT.Pure x) = FT.Pure x
    replayApF (FT.Ap x f) = FT.Ap z (replayApT f g)
      where
        z = case replay (\x y -> Just x) x y of
              Nothing -> left x
              Just k  -> right k

-- | Run @ApT f g a@ computation and record actions.
recordApT :: (Functor g, Applicative m)
          => (forall x. f x -> m (h x))     -- ^ How to run and record each particular action.
          -> (forall x. g (m x) -> m x)     -- ^ @g . m ~ m@ natural transformation.
          -> FT.ApT f g a                   -- ^ Computation to record.
          -> m (F.Ap h a)                   -- ^ The computation log wrapped in a @m@ computation.
recordApT f g (FT.ApT k) = g (recordApF <$> k)
  where
    recordApF (FT.Pure x) = pure (pure x)
    recordApF (FT.Ap x y) = (\a b -> F.liftAp a <**> b) <$> f x <*> recordApT f g y

