{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Config
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Type.Error

newtype TodoApp a = TodoApp
  { runTodo :: ReaderT Config (EitherT Error IO) a
  }
  deriving (MonadIO, MonadReader Config, Applicative, Monad, Functor, MonadError Error)

runApp :: Config -> TodoApp a -> IO (Either Error a)
runApp config = runEitherT . flip runReaderT config . runTodo