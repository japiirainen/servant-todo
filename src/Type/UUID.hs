{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Type.UUID where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.UUID (UUID, fromText, toText)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Servant
import System.Random
import Test.QuickCheck

newtype TodoUUID = TodoUUID UUID deriving (Show, Eq, Hashable)

instance ToJSON TodoUUID where
  toJSON = String . toText

instance FromJSON TodoUUID where
  parseJSON val@(String x) =
    case fromText x :: Maybe TodoUUID of
      Nothing -> typeMismatch "UUID" x
      Just vuuid -> pure vuuid
  parseJSON x = typeMismatch "UUID" x

instance Arbitrary TodoUUID where
  arbitrary = do
    num <- choose (minBound, maxBound)
    let gen = mkStdGen num
        (uuid, _) = random gen
    return $ TodoUUID uuid

instance ToText TodoUUID where
  toText (TodoUUID x) = toText x

instance FromText TodoUUID where
  fromText x = TodoUUID <$> fromText x

nextUUID :: MonadIO m => m TodoUUID
nextUUID = TodoUUID <$> liftIO UUID.nextRandom