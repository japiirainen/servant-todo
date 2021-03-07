{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App
  ( TodoApp (..),
    Config (..),
    app,
  )
where

import API
import Config (Config(..))
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Core
import Network.Wai
import Servant
import Type.Error

app :: Config -> Application
app cfg = serve (Proxy :: Proxy API) server
    where
        server :: Server API
        server enter todoToEither todoEndpoints

        todoToEither :: TodoApp :~> EitherT ServantErr IO
        todoToEither = Nat $ flip bimapEitherT id errorToServantErr
                             . flip runReaderT cfg . runTodo

        errorToServantErr :: Error ServantErr
        errorToServantErr = const err500