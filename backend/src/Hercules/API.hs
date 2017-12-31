{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API
  ( API
  , QueryAPI
  , Unprotected
  , Protected
  , UserId(..)
  ) where

import Data.Text
import Servant
import Servant.Auth.Server
import Servant.HTML.Blaze
import Servant.Swagger.UI
import Text.Blaze.Html5

import Hercules.Database.Extra       (Project, ProjectWithJobsets)
import Hercules.Database.Hercules    (GithubRepo)
import Hercules.OAuth.Authenticators (AuthenticatorName)
import Hercules.OAuth.Types          (AuthClientState, AuthCode, AuthError,
                                      AuthStatePacked, FrontendURL)
import Hercules.OAuth.User
import Hercules.Hooks.GitHub

type Unprotected =
      "projectNames" :> Get '[JSON] [Text]
 :<|> "project" :> Get '[JSON] [Project]
 :<|> "project" :> Capture "projectName" Text :> Get '[JSON] (Maybe Project)
 :<|> "projectWithJobsets" :> Get '[JSON] [ProjectWithJobsets]

type Protected = "protected" :> (
  "user" :> Get '[JSON] Text
  :<|> "user" :> "sync_repos" :> Post '[JSON] NoContent
  :<|> "repos" :> QueryParam "active" Bool :> Get '[JSON] [GithubRepo]
  :<|> "repos" :> Capture "repoId" Int :> (
      Get '[JSON] GithubRepo
      :<|> ReqBody '[JSON] GithubRepo :> Post '[JSON] GithubRepo
      :<|> "trigger" :> Capture "branch" Text :> Post '[JSON] NoContent
      )
  )

type QueryAPI = Unprotected
      :<|> Auth '[JWT] UserId :> Protected
      :<|> GitHubAppAPI

-- | A bunch of pages used for debugging and examples
type Pages = "login" :> Get '[HTML] Html
        :<|> "login" :> Capture "authType" AuthenticatorName
                     :> QueryParam "state" AuthClientState
                     :> QueryParam "frontendURL" FrontendURL
                     :> Get '[HTML] Html
        :<|> "auth-callback" :> Capture "authType" AuthenticatorName
                             :> QueryParam "code" AuthCode
                             :> QueryParam "error" AuthError
                             :> QueryParam "state" AuthStatePacked
                             :> Get '[HTML] Html
        :<|> "logged-in" :> QueryParam "jwt" Text
                         :> Get '[HTML] Html
        :<|> "repos" :> Auth '[JWT] UserId :> Get '[HTML] Html

type API = (QueryAPI
      :<|> Pages
      -- TODO: Waiting for Servant to gain Redirect combinators,
      -- The return type is wrong, this endpoint always redirects
      -- See https://github.com/haskell-servant/servant/issues/117
      :<|> Get '[HTML] Html)
      :<|> SwaggerSchemaUI "docs" "swagger.json"
