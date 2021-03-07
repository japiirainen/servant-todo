{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module DB.Todo where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Time.Clock.POSIX
import Test.QuickCheck.Arbitrary
import Type.Todo
import Type.UUID
import Type.User

newtype TodoDB = TodoDB
  { _todos :: H.HashMap UserId (H.HashMap TodoId Todo)
  }
  deriving (Show, Eq, Monoid, Arbitrary)

newTodoToTodo :: MonadIO m => NewTodo -> UserId -> m Todo
newTodoToTodo (NewTodo desc) userId = do
  created <- Created . round . (* 1000) <$> liftIO getPOSIXTime
  todoId <- TodoId <$> nextUUID
  completed <- pure $ Completed False
  description <- pure $ Description desc
  return Todo {..}

addTodo :: MonadIO m => UserId -> Todo -> TVar TodoDB -> m Todo
addTodo uid todo tvar = liftIO $ do
  atomically $ modifyTVar tvar $ addTodo' uid todo
  return todo
  where
    addTodo' :: UserId -> Todo -> TodoDB -> TodoDB
    addTodo' uid todo@Todo {..} (TodoDB tododb) =
      TodoDB $ H.insertWith H.union uid (H.fromList [(todoId, todo)]) tododb

getTodo :: MonadIO m => UserId -> TodoId -> TVar TodoDB -> m (Maybe Todo)
getTodo uid todoid tvar = do
  tododb <- liftIO $ atomically $ readTVarIO tvar
  return $ getTodo' uid todoid tododb
  where
    getTodo' :: UserId -> TodoId -> TodoDB -> Maybe Todo
    getTodo' uid todoid (TodoDB tododb) =
      H.lookup todoid =<< H.lookup uid tododb

getTodos :: MonadIO m => UserId -> TVar TodoDB -> m [Todo]
getTodos uid tvar = do
  tododb <- liftIO $ atomically $ readTVarIO tvar
  return $ getTodos' uid tododb
  where
    getTodos' :: UserId -> TodoDB -> [Todo]
    getTodos' uid (TodoDB tododb) =
      concat . maybeToList H.elems <$> H.lookup uid tododb

getTodoCount :: MonadIO m => UserId -> TVar TodoDB -> m TodoCount
getTodoCount uid tvar = do
  tododb <- liftIO $ atomically $ readTVarIO tvar
  return $ getTodoCount' uid tododb
  where
    getTodoCount' :: UserId -> TodoDB -> TodoCount
    getTodoCount' uid (TodoDB tododb) =
      fromMaybe (TodoCount 0) $
        TodoCount $ fromInteger . H.size <$> H.lookup uid tododb

updateTodo :: MonadIO m => UserId -> TodoId -> NewTodo -> TVar TodoDB -> m (Maybe Todo)
updateTodo uid todoid (NewTodo desc) tvar = do
  todo <- getTodo uid todoId tvar
  case todo of
    Nothing -> return Nothing
    Just td -> Just <$> addTodo uid (td {description = Description desc}) tvar
