{-# LANGUAGE RankNTypes #-}
module Control.Monad.Trans.Free.Replay where

import Control.Replay.Class

import Control.Monad.Trans.Class
import Control.Monad.Free.Class
import qualified Control.Monad.Trans.Free as FT
import qualified Control.Monad.Free as F

-- | Replay @FreeT f m a@ computation given @Free g ()@ computation log tree.
-- The result is new @FreeT g m@ log tree with leftover @FreeT f m@ computations and
-- unmatched @Free g ()@ log subtrees in leaves.
replayFreeT :: (Replay f g, Functor f, Functor g, Monad m)
            => FT.FreeT f m a                                         -- ^ The computation to replay.
            -> F.Free g b                                             -- ^ The log tree.
            -> FT.FreeT g m (FT.FreeT f m a, F.Free g b)
replayFreeT ft r@(F.Pure _) = return (ft, r)
replayFreeT (FT.FreeT m) r@(F.Free g) = do
  f <- lift m
  case f of
    FT.Pure x -> return (return x, r)
    FT.Free h ->
      case replay (\x y -> Just (x, y)) h g of
        Nothing -> return (wrap h, r)
        Just k -> wrap $ fmap (uncurry replayFreeT) k

-- | Run @FreeT f m a@ computation and record actions.
-- The result is a @FreeT g m a@ computation.
--
-- This function is analogous to 'iterT'.
recordFreeT :: (Functor f, Functor g, Monad m)
            => (forall x. f (m x) -> m (g x))   -- ^ How to record each layer of computation.
            -> FT.FreeT f m a                   -- ^ Computation to record.
            -> m (F.Free g a)                   -- ^ The computation log tree.
recordFreeT mapF (FT.FreeT m) = do
  f <- m
  case fmap (recordFreeT mapF) f of
    FT.Pure x -> return (return x)
    FT.Free g -> mapF g >>= return . wrap

