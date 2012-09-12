{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (c) 2005-2011 Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

module UI.HSCurses.MonadException where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Control.Exception
import Control.Monad.State

class Monad m => MonadExc m where
    catchM      :: Exception e => m a -> (e -> m a) -> m a
    blockM      :: m a -> m a
    unblockM    :: m a -> m a


class (MonadIO m, MonadExc m) => MonadExcIO m

--
-- Operations implemented in term of catchM, blockM and unblockM
-- (taken from Control.Exception).
--

catchJustM :: (Exception e, MonadExc m) =>
       (e -> Maybe b) -- ^ Predicate to select exceptions
    -> m a                    -- ^ Computation to run
    -> (b -> m a)             -- ^ Handler
    -> m a
catchJustM p a handler = catchM a handler'
  where handler' e = case p e of
            Nothing -> throw e
            Just b  -> handler b

handleM :: (Exception e, MonadExc m) => (e -> m a) -> m a -> m a
handleM = flip catchM

handleJustM :: (Exception e,MonadExc m) =>
              (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJustM p = flip (catchJustM p)

tryM :: (Exception e, MonadExc m) => m a -> m (Either e a)
tryM a = catchM (a >>= \ v -> return (Right v)) (\e -> return (Left e))

tryJustM :: (Exception e, MonadExc m) => (e -> Maybe b) -> m a -> m (Either b a)
tryJustM p a = do
  r <- tryM a
  case r of
    Right v -> return (Right v)
    Left  e -> case p e of
            Nothing -> throw e
            Just b  -> return (Left b)

bracketM :: MonadExc m =>
       m a         -- ^ computation to run first (\"acquire resource\")
    -> (a -> m b)  -- ^ computation to run last (\"release resource\")
    -> (a -> m c)  -- ^ computation to run in-between
    -> m c         -- returns the value from the in-between computation
bracketM before after thing =
  blockM (do
    a <- before
    r <- catchM
       (unblockM (thing a))
       (\(e::SomeException) -> do { after a; throw e })
    after a
    return r
  )

bracketM_ :: MonadExc m => m a -> m b -> m c -> m c
bracketM_ before after thing = bracketM before (const after) (const thing)

finally :: IO a -- ^ computation to run first
    -> IO b     -- ^ computation to run afterward (even if an exception
                --   was raised)
    -> IO a     -- returns the value from the first computation
a `finally` sequel =
  blockM (do
    r <- catchM
         (unblockM a)
         (\(e::SomeException) -> do { sequel; throw e })
    sequel
    return r
  )


--
-- Instance declarations
--

instance MonadExc IO where
    catchM       = catch
    blockM       = block
    unblockM     = unblock

instance MonadExcIO IO

instance MonadExc m => MonadExc (StateT s m) where
    catchM   = catchState
    blockM   = blockState
    unblockM = unblockState

instance (MonadExc m, MonadIO m) => MonadExcIO (StateT s m)

modifyState :: MonadExc m => (s -> m (a, s)) -> StateT s m a
modifyState f =
    do oldState <- get
       (x, newState) <- lift $ f oldState
       put newState
       return x

catchState :: (Exception e, MonadExc m)
           => StateT s m a -> (e -> StateT s m a) -> StateT s m a
catchState run handler =
    modifyState (\oldState -> runStateT run oldState `catchM`
                              (\e -> runStateT (handler e) oldState))

blockState, unblockState :: (MonadExc m) => StateT s m a -> StateT s m a
blockState run =
    modifyState (\oldState -> blockM (runStateT run oldState))

unblockState run =
    modifyState (\oldState -> unblockM (runStateT run oldState))
