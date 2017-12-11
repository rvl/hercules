{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  , swaggerDoc
  , server
  , appContext
  , appToHandler
  ) where

import Control.Monad                        (join)
import Control.Monad.Except
import Control.Monad.Log
import Control.Natural
import Data.Bifunctor                       (second)
import Data.Foldable                        (toList)
import Data.List                            (sortOn)
import Data.Maybe                           (catMaybes)
import Data.Monoid                          ((<>))
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Safe                                 (headMay)
import Servant
import Servant.Utils.Enter                  (enter)
import Servant.Auth.Server                  (AuthResult (..), CookieSettings, JWTSettings,
                                             defaultCookieSettings)
import Servant.Mandatory
import Servant.Redirect
import Servant.Swagger.UI

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO       as T

import Hercules.API
import Hercules.Config
import Hercules.Database.Extra       (JobsetNullable, Project,
                                      ProjectWithJobsets (..),
                                      fromNullableJobset, projectName)
import Hercules.OAuth
import Hercules.OAuth.Authenticators
import Hercules.Query.Hydra
import Hercules.ServerEnv
import Hercules.Static
import Hercules.Swagger
import Hercules.Hooks.GitHub

startApp :: Config -> IO ()
startApp config = do
  let authenticators = configAuthenticatorList config
      port = configPort config
      logging = loggingMiddleware config
  newEnv config authenticators >>= \case
    Nothing -> pure ()
    Just env -> do
      T.putStrLn $ "Serving on http://" <> configHostname config
                   <> ":" <> (pack . show $ port)
      run port . logging =<< app env

loggingMiddleware :: Config -> Middleware
loggingMiddleware config = case configAccessLogLevel config of
  Disabled    -> id
  Enabled     -> logStdout
  Development -> logStdoutDev

app :: Env -> IO Application
app env = pure $ serveWithContext api (appContext env) (server env)
  where api = Proxy :: Proxy API

appContext :: Env -> Context '[GitHubKey, CookieSettings, JWTSettings]
appContext env = gitHubWebHookCtx env :. defaultCookieSettings :. envJWTSettings env :. EmptyContext

appToHandler' :: forall a. Env -> App a -> Servant.Handler a
appToHandler' env r = do
  res <- liftIO $ runExceptT (runApp env r)
  case res of
    Left err -> throwError err
    Right a -> return a

appToHandler :: Env -> App :~> Servant.Handler
appToHandler env = NT (appToHandler' env)

server :: Env -> Server API
server env = enter (appToHandler env) api :<|> serveSwagger
  where api = queryApi
              :<|> pages
              :<|> root
        pages = welcomePage
                :<|> (mandatory1 .: loginPage)
                :<|> (mandatory1 .∵ authCallback)
                :<|> loggedInPage
                :<|> (join . withAuthenticated userInfoPage)
        queryApi = unprotected :<|> protected :<|> gitHubAppApi
        unprotected = getProjectNames
                      :<|> getProjects
                      :<|> getProject
                      :<|> getProjectsWithJobsets
        protected = getUser

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.∵) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.∵) = (.) . (.) . (.)


root :: App a
root = redirectBS "/docs/"

serveSwagger :: Server (SwaggerSchemaUI "docs" "swagger.json")
serveSwagger = swaggerSchemaUIServer swaggerDoc

getUser :: AuthResult UserId -> App Text
getUser = withAuthenticated (pack . show)

withAuthenticated :: (a -> b) -> AuthResult a -> App b
withAuthenticated f = \case
  (Authenticated x) -> pure (f x)
  _                 -> do
    logNotice "Failed user authentication attempt"
    throwError err401

getProjectNames :: App [Text]
getProjectNames = runHerculesQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runHerculesQueryWithConnection (projectQuery name)

getProjects :: App [Project]
getProjects = runHerculesQueryWithConnection projectsQuery

getProjectsWithJobsets :: App [ProjectWithJobsets]
getProjectsWithJobsets =
  fmap (uncurry makeProjectWithJobsets . second toList)
  . groupSortOn projectName
  <$> (runHerculesQueryWithConnection projectsWithJobsetsQuery :: App [(Project, JobsetNullable)])
  where
    makeProjectWithJobsets :: Project -> [JobsetNullable] -> ProjectWithJobsets
    makeProjectWithJobsets p jms =
      let js = catMaybes (fromNullableJobset <$> jms)
      in ProjectWithJobsets p js

groupSortOn :: Ord k => (a -> k) -> [(a, v)] -> [(a, NE.NonEmpty v)]
groupSortOn f = fmap (\x -> (fst $ NE.head x, fmap snd x))
          . NE.groupWith (f . fst)
          . sortOn (f . fst)
