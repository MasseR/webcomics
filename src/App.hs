module App where

import Database
import Spider
import Network.Wreq.Session (Session)

data App = App { pool :: Pool SqlBackend
               , session :: Session}

instance HasSession App where
    getSession = session

instance HasBackend App where
    getBackend = pool
