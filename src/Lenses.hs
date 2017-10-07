{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Lenses where

import Config
import Lens.Micro.TH

makeFields ''PostgresConfig
makeFields ''Config
