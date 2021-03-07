{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.User
  ( UserAPI,
    userAPI,
  )
where

import Config
import Control.Monad.Reader
import Core
import DB.User
import Servant
import Type.User

type UserAPI = "user" :> ReqBody '[JSON] LoginUser :> Post '[JSON] User

userAPI :: ServerT UserAPI TodoApp
userAPI user = getUser user =<< asks userdb