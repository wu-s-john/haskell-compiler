{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.Maybe where

import Control.Monad (void)
import Control.Monad.Extra (unlessM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (isJust,fromMaybe)

runNothing :: Monad m => MaybeT m a -> m () -> MaybeT m ()
runNothing maybeMonad action = do
  lift $ unlessM (isJust <$> runMaybeT maybeMonad) action
  void maybeMonad

runMaybe :: Monad m => a -> MaybeT m a -> m a
runMaybe defaultValue action = do
  maybeValue <- runMaybeT action
  return $ fromMaybe defaultValue maybeValue

killComputation :: Monad m => MaybeT m () -> MaybeT m ()
killComputation computation = computation >> (MaybeT $ return Nothing)

continueComputation :: Monad m => MaybeT m () -> MaybeT m ()
continueComputation computation =
  MaybeT $ do
    unlessM (isJust <$> runMaybeT computation) (return ())
    return (Just ())
