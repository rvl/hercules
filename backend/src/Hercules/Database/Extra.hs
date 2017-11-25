{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hercules.Database.Extra
  ( ProjectWithJobsets(..)
  , module Hercules.Database.Hydra
  , Jobset'(..), Jobset, JobsetNullable, fromNullableJobset
  ) where

import Data.Aeson
import GHC.Generics
import Hercules.Database.Hydra
import Hercules.Database.Hercules
import Servant.Elm

data ProjectWithJobsets = ProjectWithJobsets
  { projectWithJobsetsProject :: Project
  , projectWithJobsetsJobsets :: [Jobset]
  }
  deriving(Generic)

instance ToJSON ProjectWithJobsets where
instance ElmType ProjectWithJobsets where
