{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Lenses where

import Config
import Database.Migration
import Lens.Micro.TH

makeFields ''PostgresConfig
makeFields ''Config
makeFields ''Comic
makeFields ''Page
