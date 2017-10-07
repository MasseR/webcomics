{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Database.Migration where

import Database.Persist.TH
import Database.Persist
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Comic
    name Text
Page
    previous Text Maybe
    next Text Maybe
    image Text Maybe
    comic ComicId
        |]
