{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main
  ( main
  ) where

import Data.Text (Text, replace, pack)
import Data.Monoid ((<>))
import Elm
import Servant.Auth.Server
import Servant.GitHub.Webhook
import Servant.Elm
import Servant.Foreign
import Servant.API.TypeLevel (Elem)
import Options.Applicative

import Hercules.API
import Hercules.Database.Extra


elmoptions :: Options
elmoptions = Options {fieldLabelModifier = replace "'" ""}

spec :: ElmOptions -> Spec
spec elmexportoptions = Spec ["Hercules"]
            (defElmImports
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Project)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Project)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobset)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobset)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy ProjectWithJobsets)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy ProjectWithJobsets)
              : generateElmForAPIWith elmexportoptions (Proxy :: Proxy QueryAPI)
            )

-- Generate Authorization header for Elm protected URLs
-- https://github.com/plow-technologies/servant-auth/issues/8
instance forall lang ftype api auths a.
    ( HasForeign lang ftype api
    , HasForeignType lang ftype Text
    , JWT `Elem` auths
    )
  => HasForeign lang ftype (Auth auths a :> api) where
  type Foreign ftype (Auth auths a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
      arg = Arg
        { _argName = PathSegment "authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
        }

-- Provide stub instance for github webhooks, although these would
-- never be called from elm.
instance forall lang ftype api evs. HasForeign lang ftype api
  => HasForeign lang ftype (GitHubEvent evs :> api) where
  type Foreign ftype (GitHubEvent evs :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR

-- Provide stub instance for github webhook auth protected apis.
instance forall lang ftype api ev b. HasForeign lang ftype api
  => HasForeign lang ftype (GitHubSignedReqBody b ev :> api) where
  type Foreign ftype (GitHubSignedReqBody b ev :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR


data ElmConfig = ElmConfig
  { elmpath :: String
  }

parser :: Parser ElmConfig
parser =
      ElmConfig
  <$> argument str (metavar "FOLDER")

main :: IO ()
main = do
  elmconfig <- execParser $ info (helper <*> parser)
    (fullDesc <> progDesc "Generate types for Elm frontend")
  let elmexportoptions = defElmOptions { elmExportOptions = elmoptions , urlPrefix = Dynamic }
  specsToDir [spec elmexportoptions] $ elmpath elmconfig
