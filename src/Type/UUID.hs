{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.UUID where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Servant as S
import System.Random
import Test.QuickCheck

newtype TodoUUID = TodoUUID UUID
  deriving (Show, Eq, Hashable, ToJSON, FromJSON)

instance Arbitrary TodoUUID where
  arbitrary = do
    num <- choose (minBound, maxBound)
    let gen = mkStdGen num
        (uuid, _) = random gen
    return $ TodoUUID uuid

nextUUID :: MonadIO m => m TodoUUID
nextUUID = TodoUUID <$> liftIO UUID.nextRandom