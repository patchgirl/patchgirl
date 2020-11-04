{-# LANGUAGE OverloadedStrings #-}

module DevelMain where

import           Prelude

import           Control.Concurrent (MVar, ThreadId, forkIO, killThread,
                                     newEmptyMVar, putMVar, takeMVar)
import           Control.Exception  (finally)
import           Control.Monad      ((>=>))
import           Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import           Foreign.Store      (Store (..), lookupStore, readStore,
                                     storeAction, withStore)
import           GHC.Word           (Word32)

import           Server

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
  putStrLn "Updating"
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> do
      putStrLn "No server is running "
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    -- server is already running
    Just tidStore -> do
      putStrLn "Server is already running "
      restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start

    -- | Start the server in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done =
        forkIO (finally run
                        -- Note that this implies concurrency
                        -- between shutdownApp and the next app that is starting.
                        -- Normally this should be fine
                        (putMVar done ()))

-- | kill the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
