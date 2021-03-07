{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Config where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import DB.Todo
import DB.User
import System.Envy

data Config = Config
  { port :: Int,
    tododb :: TVar TodoDB,
    userdb :: TVar UserDB
  }

instance FromEnv Config where
  fromEnv =
    Config <$> envMaybe "TODO_PORT" .!= 8000
      <*> (liftIO $ atomically (newTVarIO defTodoDB))
      <*> (liftIO $ atomically (newTVarIO mempty))
