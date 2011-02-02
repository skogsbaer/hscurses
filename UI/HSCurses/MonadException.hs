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

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.State
import Data.Dynamic

class Monad m => MonadExc m where
    catchM      :: m a -> (Exception -> m a) -> m a
    blockM      :: m a -> m a
    unblockM    :: m a -> m a


class (MonadIO m, MonadExc m) => MonadExcIO m

--
-- Operations implemented in term of catchM, blockM and unblockM
-- (taken from Control.Exception).
--

catchJustM :: MonadExc m =>
       (Exception -> Maybe b) -- ^ Predicate to select exceptions
    -> m a                    -- ^ Computation to run
    -> (b -> m a)             -- ^ Handler
    -> m a
catchJustM p a handler = catchM a handler'
  where handler' e = case p e of
            Nothing -> throw e
            Just b  -> handler b

handleM :: MonadExc m => (Exception -> m a) -> m a -> m a
handleM = flip catchM

handleJustM :: MonadExc m =>
              (Exception -> Maybe b) -> (b -> m a) -> m a -> m a
handleJustM p = flip (catchJustM p)

tryM :: MonadExc m => m a -> m (Either Exception a)
tryM a = catchM (a >>= \ v -> return (Right v)) (\e -> return (Left e))

tryJustM :: MonadExc m => (Exception -> Maybe b) -> m a -> m (Either b a)
tryJustM p a = do
  r <- tryM a
  case r of
    Right v -> return (Right v)
    Left  e -> case p e of
            Nothing -> throw e
            Just b  -> return (Left b)

catchDynM :: (MonadExc m, Typeable exc) =>
             m a -> (exc -> m a) -> m a
catchDynM m k = catchM m handl
  where handl ex = case ex of
               (DynException dyn) ->
                case fromDynamic dyn of
                    Just exception  -> k exception
                    Nothing -> throw ex
               _ -> throw ex

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
       (\e -> do { after a; throw e })
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
         (\e -> do { sequel; throw e })
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

catchState :: (MonadExc m) => StateT s m a -> (Exception -> StateT s m a)
           -> StateT s m a
catchState run handler =
    modifyState (\oldState -> runStateT run oldState `catchM`
                              (\e -> runStateT (handler e) oldState))

blockState, unblockState :: (MonadExc m) => StateT s m a -> StateT s m a
blockState run =
    modifyState (\oldState -> blockM (runStateT run oldState))

unblockState run =
    modifyState (\oldState -> unblockM (runStateT run oldState))

