{-# Language DeriveGeneric #-}
module Config where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Yaml.Config as Y
import GHC.Generics (Generic)

data PostgresConfig = PostgresConfig { postgresConfigUsername :: String
                                     , postgresConfigPassword :: String
                                     , postgresConfigHostname :: String
                                     , postgresConfigPort :: Int } deriving (Show, Generic)

data Config = Config { configPostgres :: PostgresConfig } deriving (Show, Generic)

instance FromJSON PostgresConfig where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 14}

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6}

readConfig :: IO Config
readConfig = Y.loadYamlSettings ["config/config.yaml"] [] Y.useEnv
