{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Todo
  ( TodoAPI,
    todoAPI,
  )
where

import Config
import Control.Monad.Reader
import Core
import DB.Todo
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Servant
import Servant.Client
import Servant.Server.Internal
import Type.Todo
import Type.User
import qualified Web.JWT as JWT

type TodoAPI =
  "todo" :> AuthToken :> QueryParam "orderby" OrderBy :> QueryParam "completed" Completed :> Get '[JSON] [Todo]
    :<|> "todo" :> AuthToken :> Capture "id" TodoId :> Get '[JSON] (Maybe Todo)
    :<|> "todo" :> AuthToken :> Capture "id" TodoId :> Delete '[JSON] ()
    :<|> "todo" :> AuthToken :> Capture "id" TodoId :> ReqBody '[JSON] NewTodo :> Put '[JSON] (Maybe Todo)
    :<|> "todo" :> AuthToken :> "count" :> Get '[JSON] TodoCount
    :<|> "todo" :> AuthToken :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo

todoAPI :: ServerT TodoAPI TodoApp
todoAPI = todoGetAll :<|> todoGet :<|> todoDelete :<|> todoUpdate :<|> todoCount :<|> todoCreate

todoGet :: UserId -> TodoId -> TodoApp (Maybe Todo)
todoGet uid todoid = getTodo uid todoid =<< asks tododb

todoCount :: UserId -> TodoApp TodoCount
todoCount uid = getTodoCount uid =<< asks tododb

todoGetAll :: UserId -> Maybe OrderBy -> Maybe Completed -> TodoApp [Todo]
todoGetAll uid _ _ = getTodos uid =<< asks tododb

todoDelete :: UserId -> TodoId -> TodoApp ()
todoDelete uid todoid = deleteTodo uid todoid =<< asks tododb

todoUpdate :: UserId -> TodoId -> NewTodo -> TodoApp (Maybe Todo)
todoUpdate uid todoid newtodo = updateTodo uid todoid newtodo =<< asks tododb

instance HasServer api => HasServer (AuthToken :> api) where
  type ServerT (AuthToken :> api) m = UserId -> ServerT api m
  route Proxy subServer req@Request {..} resp =
    case getKey req of
      Nothing -> the401
      Just userid -> route (Proxy :: Proxy api) (subServer userid) req resp
    where
      the401 = resp . succeedWith $ responseLBS status401 [] "Invalid or missing Token"
      getKey :: Request -> Maybe UserId
      getKey Request {..} = do
        key <- lookup "X-Access-Token" requestHeaders
        sub <-
          JWT.sub . JWT.claims
            <$> JWT.decodeAndVerifySignature (JWT.hmacSecret "secret") (T.decodeUtf8 key)
        fromText . JWT.stringOrURIToText =<< sub