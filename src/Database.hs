{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language RecordWildCards #-}
module Database where

import Database.Persist.TH
import Database.Persist
import Database.Persist.Postgresql
import Data.Text (Text)
import Control.Monad.Trans
import Control.Monad.Reader
import Config (PostgresConfig(..))
import Data.ByteString.Char8 (pack, ByteString)

createConnectionString :: PostgresConfig -> ByteString
createConnectionString PostgresConfig{..} =
    pack $ unwords [ "host="++postgresConfigHostname
                   , "port="++(show postgresConfigPort)
                   , "user="++postgresConfigUsername
                   , "password="++postgresConfigPassword]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Comic
    name Text
        |]

dumpMigration :: ReaderT SqlBackend IO ()
dumpMigration = printMigration migrateAll
