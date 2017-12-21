{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.Swagger
  ( swaggerDoc
  ) where

import Control.Lens            hiding ((.=))
import Data.Proxy
import Data.Swagger
import Servant
import Servant.Auth.Swagger    ()
import Servant.Swagger
import Servant.GitHub.Webhook
import GHC.TypeLits (KnownSymbol)

import Hercules.API            (QueryAPI)
import Hercules.Database.Extra
import Hercules.Database.Hercules (GithubApp, GithubRepo)
import Hercules.Hooks.GitHub

instance ToSchema Project where
instance ToSchema Jobset where
instance ToSchema ProjectWithJobsets where
instance ToSchema GithubApp where
instance ToSchema GithubRepo

instance HasSwagger sub => HasSwagger (GitHubEvent evs :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

instance HasSwagger sub => HasSwagger (GitHubSignedReqBody b ev :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy QueryAPI)
    & info.title       .~ "Hercules CI API"
    & info.version     .~ "1.0"
    & info.description ?~ ""
