{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Applicative
import Control.Monad.Free (Free)
import Control.Monad.Free.Class
import Control.Monad.Trans
import Control.Monad.Trans.Free (FreeT, iterT)
import Control.Monad.Trans.Free.Replay
import Control.Replay.Class

import Control.Concurrent
import Control.Concurrent.STM

import GHC.Generics

-- | This is our base functor, which describes the list of actions.
data F a
  = Ask (String -> a)       -- ^ Get some input.
  | Fork a (ThreadId -> a)  -- ^ Fork computation.
  | Halt                    -- ^ Abort computation.
  deriving (Functor, Generic1)

-- | This is a derived data structure which retains 'F' tree structure and
-- stores recorded values for functions in 'F'.
data F' a
  = Ask' (String, a)        -- ^ Recorded input.
  | Fork' a (ThreadId, a)   -- ^ Recorded child ThreadId.
  | Halt'                   -- ^ We don't record anything for halt.
  deriving (Show, Functor, Generic1)

-- derive Generic1 instance
instance Replay F F'

data WithSave f a
  = Save            -- ^ Pause computation to collect the log.
  | Continue (f a)  -- ^ Continue computation.
  deriving (Show, Functor)

instance Replay f g => Replay f (WithSave g) where
  replay f x (Continue y) = Continue <$> replay f x y
  replay _ _ _ = empty

-- DSL commands for F functor
--
-- Note: these can be actually derived automatically using $(makeFree ''F)

-- | Ask user for input.
ask :: MonadFree F m => m String
ask = liftF $ Ask id

-- | Fork computation.
fork :: MonadFree F m => m (Maybe ThreadId)
fork = liftF $ Fork Nothing Just

-- | Halt computation.
halt :: MonadFree F m => m a
halt = liftF Halt

-- | Perform and record an F action in IO monad.
recordF :: F (IO a) -> IO (WithSave F' a)
recordF (Ask g) = do
  s <- getLine
  case s of
    "save" -> return Save
    _ -> do
      x <- g s
      return (Continue (Ask' (s, x)))
recordF (Fork c p) = do
  v <- atomically newEmptyTMVar
  pid <- forkIO $ c >>= atomically . putTMVar v
  px <- p pid
  cx <- atomically $ takeTMVar v
  return (Continue (Fork' cx (pid, px)))
recordF Halt = return (Continue Halt')

-- | Perform recorded actions (simplified).
evalF' :: F' (IO a) -> IO a
evalF' (Ask' (_, m)) = m
evalF' (Fork' mc (_, mp)) = do
  v <- atomically newEmptyTMVar
  _ <- forkIO $ mc >> atomically (putTMVar v ())
  x <- mp
  _ <- atomically $ takeTMVar v -- this is simply waiting for another thread to finish
  return x
evalF' Halt' = error "halt"

evalWithSaveF' :: WithSave F' (IO a) -> IO a
evalWithSaveF' Save         = error "halt"
evalWithSaveF' (Continue x) = evalF' x

-- | Sample program.
test :: (MonadFree F m, MonadIO m) => m ()
test = do
  name <- prompt "What's your name?"
  liftIO $ putStrLn ("Hello, " ++ name ++ "!")

  x <- prompt "What do you want to do (save/halt/continue)?"
  case x of
    "halt" -> halt
    _ -> liftIO $ putStrLn "Continuing..."

  mpid <- fork
  liftIO . putStrLn $
    case mpid of
      Nothing   -> "I am child!"
      Just pid  -> "I am parent! My child is " ++ show pid ++ "."

  y <- prompt $ show mpid ++ ": And the final input!"
  liftIO $ putStrLn y
  where
    prompt s = do
      liftIO $ putStrLn s
      ask

main :: IO ()
main = do
  putStrLn "========================================"
  putStrLn "  Recording"
  putStrLn "========================================"

  logTree <- recordFreeT recordF test

  putStrLn "========================================"
  print logTree

  putStrLn "========================================"
  putStrLn "  Replaying"
  putStrLn "========================================"

  let -- build a replayed computation
      replayed   = replayFreeT test logTree
      -- evaluate leftover computations at the leaves of replayed computation
      -- we ignore unmatched logTree subtrees
      replayed'  = fmap (recordFreeT recordF . fst) replayed
      -- attach computations at leaves to the computation tree
      replayed'' :: FreeT (WithSave F') IO (Free (WithSave F') ())
      replayed'' = do
        m <- replayed'
        lift $ do
          putStrLn "========================================"
          putStrLn "  Continuing"
          putStrLn "========================================"
          m
  -- replay and continue computation
  _logTree <- iterT evalWithSaveF' replayed''

  putStrLn "========================================"

  return ()

