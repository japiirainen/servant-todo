{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module DB.User where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.HashMap.Strict as H
import Data.Time.Clock.POSIX
import Data.UUID (toText)
import Type.UUID
import Type.User
import qualified Web.JWT as JWT

newtype UserDB = UserDB
  { _users :: H.HashMap UserName User
  }
  deriving (Show, Eq, Monoid)

createToken (UserId uid) = do
  let claim :: JWT.JWTClaimsSet
      claim =
        JWT.nbf
          { JWT.sub = JWT.stringOrURI (toText uid)
          }
  return $ JWT.encodeSigned JWT.HS256 (JWT.hmacSecret "secret") claim

getUser :: MonadIO m => LoginUser -> TVar UserDB -> m User
getUser lu@LoginUser {..} tvar = liftIO $ do
  UserDB db <- atomically $ readTVarIO tvar
  case H.lookup user db of
    Just u -> return u
    Nothing -> do
      u <- registerUser lu
      atomically $ modifyTVar tvar $ \(UserDB db) -> UserDB $ H.insert user u db
      return u

registerUser :: MonadIO m => LoginUser -> m User
registerUser LoginUser {..} = liftIO $ do
  let username = user
      password = pass
  userId <- UserId <$> nextUUID
  token <- AuthToken <$> createToken userId
  return User {..}