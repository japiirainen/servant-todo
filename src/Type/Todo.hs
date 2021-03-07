{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Todo where

import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import qualified Data.UUID.V4 as UUID
import GHC.Generics
import Test.QuickCheck
import Type.UUID
import Type.User

newtype Created = Created Integer
  deriving (Show, Eq, ToJSON, FromJSON, Arbitrary)

newtype Completed = Completed Bool
  deriving (Show, Eq, ToJSON, FromJSON, Arbitrary)

newtype Description = Description Text
  deriving (Show, Eq, ToJSON, FromJSON)

newtype TodoCount = TodoCount Integer
  deriving (Show, Eq, ToJSON, FromJSON)

newtype NewTodo = NewTodo Text
  deriving (Show, Eq)

instance ToJSON NewTodo where
  toJSON (NewTodo todo) = object ["todo" .= todo]

instance FromJSON NewTodo where
  parseJSON (Object o) = NewTodo <$> o .: "todo"

instance Arbitrary TodoCount where
  arbitrary = TodoCount . abs <$> arbitrary

data OrderBy = Asc | Desc
  deriving (Show, Eq)

newtype TodoId = TodoId TodoUUID
  deriving (Show, Eq, FromJSON, ToJSON, Arbitrary, Hashable)

data Todo = Todo
  { userId :: UserId,
    todoId :: TodoId,
    created :: Created,
    completed :: Completed,
    description :: Description
  }
  deriving (Show, Eq, Generic)

instance ToJSON Todo

instance FromJSON Todo