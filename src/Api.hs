{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API (TodoAPI, todoEndpoints, API) where

import Core
import Servant
import Web.Todo
import Web.User

type API = UserAPI :<|> TodoAPI

todoEndpoints :: ServerT API TodoApp
todoEndpoints = userAPI :<|> todoAPI
