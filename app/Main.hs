module Main (main) where

import API
import App
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant.Server
import System.Environment
import System.Envy

main :: IO ()
main = do
  result <- decodeEnv
  let api = Proxy :: Proxy API
  case result of
    Left errMsg -> print errMsg
    Right config@Config {..} -> do
      putStrLn $ "Running sever on " ++ show port ++ "..."
      run port (app config)
