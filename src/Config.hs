module Config where

data PostgresConfig = PostgresConfig { postgresConfigUsername :: String
                                     , postgresConfigPassword :: String
                                     , postgresConfigHostname :: String
                                     , postgresConfigPort :: String } deriving Show

data Config = Config { configPostgres :: PostgresConfig } deriving Show


