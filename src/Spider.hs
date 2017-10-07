module Spider where

import Network.Wreq.Session
import Network.Wreq (responseBody)
import Rules
import Control.Monad.Logger
import Control.Monad.Reader
import Text.XML (Document)
import Text.HTML.DOM (parseLBS)
import Lens.Micro

class HasSession a where
    getSession :: a -> Session

instance HasSession Session where
    getSession = id

fetchDocument :: (HasSession r, MonadReader r m, MonadLogger m, MonadIO m) => String -> m Document
fetchDocument url = do
    session <- asks getSession
    r <- liftIO $ get session url
    return $ parseLBS (r ^. responseBody)
