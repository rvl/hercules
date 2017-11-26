{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hercules.Database.Hydra where

import           Data.Aeson
import qualified Data.Aeson                      as JSON
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text
import           GHC.Generics
import           GHC.Int
import           Opaleye                         hiding (fromNullable)
import           Servant.Elm

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: aggregateconstituents ----

data Aggregateconstituent' c1 c2 =
  Aggregateconstituent
    { aggregateconstituentAggregate   :: c1
    , aggregateconstituentConstituent :: c2
    }

type Aggregateconstituent = Aggregateconstituent' Int32 Int32

type AggregateconstituentReadColumns = Aggregateconstituent' (Column PGInt4) (Column PGInt4)

type AggregateconstituentWriteColumns = Aggregateconstituent' (Column PGInt4) (Column PGInt4)

type AggregateconstituentNullableColumns = Aggregateconstituent' (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type AggregateconstituentNullable = Aggregateconstituent' (Maybe Int32) (Maybe Int32)

fromNullableAggregateconstituent :: AggregateconstituentNullable -> Maybe Aggregateconstituent
fromNullableAggregateconstituent = fromNullable

$(makeAdaptorAndInstance "pAggregateconstituent" ''Aggregateconstituent')

aggregateconstituentTable :: Table AggregateconstituentWriteColumns AggregateconstituentReadColumns
aggregateconstituentTable = Table "aggregateconstituents" (pAggregateconstituent
  Aggregateconstituent
    { aggregateconstituentAggregate = required "aggregate"
    , aggregateconstituentConstituent = required "constituent"
    }
  )

---- Types for table: buildinputs ----

data Buildinput' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  Buildinput
    { buildinputId               :: c1
    , buildinputBuild            :: c2
    , buildinputName             :: c3
    , buildinputType             :: c4
    , buildinputUri              :: c5
    , buildinputRevision         :: c6
    , buildinputValue            :: c7
    , buildinputEmailresponsible :: c8
    , buildinputDependency       :: c9
    , buildinputPath             :: c10
    , buildinputSha256Hash       :: c11
    }

type Buildinput = Buildinput' Int32 (Maybe Int32) Text Text (Maybe Text) (Maybe Text) (Maybe Text) Int32 (Maybe Int32) (Maybe Text) (Maybe Text)

type BuildinputReadColumns = Buildinput' (Column PGInt4) (Column (Nullable PGInt4)) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildinputWriteColumns = Buildinput' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type BuildinputNullableColumns = Buildinput' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildinputNullable = Buildinput' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableBuildinput :: BuildinputNullable -> Maybe Buildinput
fromNullableBuildinput = fromNullable

$(makeAdaptorAndInstance "pBuildinput" ''Buildinput')

buildinputTable :: Table BuildinputWriteColumns BuildinputReadColumns
buildinputTable = Table "buildinputs" (pBuildinput
  Buildinput
    { buildinputId = optional "id"
    , buildinputBuild = optional "build"
    , buildinputName = required "name"
    , buildinputType = required "type"
    , buildinputUri = optional "uri"
    , buildinputRevision = optional "revision"
    , buildinputValue = optional "value"
    , buildinputEmailresponsible = required "emailresponsible"
    , buildinputDependency = optional "dependency"
    , buildinputPath = optional "path"
    , buildinputSha256Hash = optional "sha256hash"
    }
  )

---- Types for table: buildmetrics ----

data Buildmetric' c1 c2 c3 c4 c5 c6 c7 c8 =
  Buildmetric
    { buildmetricBuild     :: c1
    , buildmetricName      :: c2
    , buildmetricUnit      :: c3
    , buildmetricValue     :: c4
    , buildmetricProject   :: c5
    , buildmetricJobset    :: c6
    , buildmetricJob       :: c7
    , buildmetricTimestamp :: c8
    }

type Buildmetric = Buildmetric' Int32 Text (Maybe Text) Double Text Text Text Int32

type BuildmetricReadColumns = Buildmetric' (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column PGFloat8) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type BuildmetricWriteColumns = Buildmetric' (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGFloat8) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type BuildmetricNullableColumns = Buildmetric' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGFloat8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type BuildmetricNullable = Buildmetric' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Double) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32)

fromNullableBuildmetric :: BuildmetricNullable -> Maybe Buildmetric
fromNullableBuildmetric = fromNullable

$(makeAdaptorAndInstance "pBuildmetric" ''Buildmetric')

buildmetricTable :: Table BuildmetricWriteColumns BuildmetricReadColumns
buildmetricTable = Table "buildmetrics" (pBuildmetric
  Buildmetric
    { buildmetricBuild = required "build"
    , buildmetricName = required "name"
    , buildmetricUnit = optional "unit"
    , buildmetricValue = required "value"
    , buildmetricProject = required "project"
    , buildmetricJobset = required "jobset"
    , buildmetricJob = required "job"
    , buildmetricTimestamp = required "timestamp"
    }
  )

---- Types for table: buildoutputs ----

data Buildoutput' c1 c2 c3 =
  Buildoutput
    { buildoutputBuild :: c1
    , buildoutputName  :: c2
    , buildoutputPath  :: c3
    }

type Buildoutput = Buildoutput' Int32 Text Text

type BuildoutputReadColumns = Buildoutput' (Column PGInt4) (Column PGText) (Column PGText)

type BuildoutputWriteColumns = Buildoutput' (Column PGInt4) (Column PGText) (Column PGText)

type BuildoutputNullableColumns = Buildoutput' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildoutputNullable = Buildoutput' (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableBuildoutput :: BuildoutputNullable -> Maybe Buildoutput
fromNullableBuildoutput = fromNullable

$(makeAdaptorAndInstance "pBuildoutput" ''Buildoutput')

buildoutputTable :: Table BuildoutputWriteColumns BuildoutputReadColumns
buildoutputTable = Table "buildoutputs" (pBuildoutput
  Buildoutput
    { buildoutputBuild = required "build"
    , buildoutputName = required "name"
    , buildoutputPath = required "path"
    }
  )

---- Types for table: buildproducts ----

data Buildproduct' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  Buildproduct
    { buildproductBuild       :: c1
    , buildproductProductnr   :: c2
    , buildproductType        :: c3
    , buildproductSubtype     :: c4
    , buildproductFilesize    :: c5
    , buildproductSha1Hash    :: c6
    , buildproductSha256Hash  :: c7
    , buildproductPath        :: c8
    , buildproductName        :: c9
    , buildproductDescription :: c10
    , buildproductDefaultpath :: c11
    }

type Buildproduct = Buildproduct' Int32 Int32 Text Text (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text) Text (Maybe Text) (Maybe Text)

type BuildproductReadColumns = Buildproduct' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildproductWriteColumns = Buildproduct' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type BuildproductNullableColumns = Buildproduct' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildproductNullable = Buildproduct' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableBuildproduct :: BuildproductNullable -> Maybe Buildproduct
fromNullableBuildproduct = fromNullable

$(makeAdaptorAndInstance "pBuildproduct" ''Buildproduct')

buildproductTable :: Table BuildproductWriteColumns BuildproductReadColumns
buildproductTable = Table "buildproducts" (pBuildproduct
  Buildproduct
    { buildproductBuild = required "build"
    , buildproductProductnr = required "productnr"
    , buildproductType = required "type"
    , buildproductSubtype = required "subtype"
    , buildproductFilesize = optional "filesize"
    , buildproductSha1Hash = optional "sha1hash"
    , buildproductSha256Hash = optional "sha256hash"
    , buildproductPath = optional "path"
    , buildproductName = required "name"
    , buildproductDescription = optional "description"
    , buildproductDefaultpath = optional "defaultpath"
    }
  )

---- Types for table: builds ----

data Build' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 =
  Build
    { buildId             :: c1
    , buildFinished       :: c2
    , buildTimestamp      :: c3
    , buildProject        :: c4
    , buildJobset         :: c5
    , buildJob            :: c6
    , buildNixname        :: c7
    , buildDescription    :: c8
    , buildDrvpath        :: c9
    , buildSystem         :: c10
    , buildLicense        :: c11
    , buildHomepage       :: c12
    , buildMaintainers    :: c13
    , buildMaxsilent      :: c14
    , buildTimeout        :: c15
    , buildIschannel      :: c16
    , buildIscurrent      :: c17
    , buildNixexprinput   :: c18
    , buildNixexprpath    :: c19
    , buildPriority       :: c20
    , buildGlobalpriority :: c21
    , buildStarttime      :: c22
    , buildStoptime       :: c23
    , buildIscachedbuild  :: c24
    , buildBuildstatus    :: c25
    , buildSize           :: c26
    , buildClosuresize    :: c27
    , buildReleasename    :: c28
    , buildKeep           :: c29
    }

type Build = Build' Int32 Int32 Int32 Text Text Text (Maybe Text) (Maybe Text) Text Text (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) Int32 (Maybe Int32) (Maybe Text) (Maybe Text) Int32 Int32 (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int64) (Maybe Int64) (Maybe Text) Int32

type BuildReadColumns = Build' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGInt4) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column PGInt4)

type BuildWriteColumns = Build' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGText))) (Column PGInt4)

type BuildNullableColumns = Build' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type BuildNullable = Build' (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int64) (Maybe Int64) (Maybe Text) (Maybe Int32)

fromNullableBuild :: BuildNullable -> Maybe Build
fromNullableBuild = fromNullable

$(makeAdaptorAndInstance "pBuild" ''Build')

buildTable :: Table BuildWriteColumns BuildReadColumns
buildTable = Table "builds" (pBuild
  Build
    { buildId = optional "id"
    , buildFinished = required "finished"
    , buildTimestamp = required "timestamp"
    , buildProject = required "project"
    , buildJobset = required "jobset"
    , buildJob = required "job"
    , buildNixname = optional "nixname"
    , buildDescription = optional "description"
    , buildDrvpath = required "drvpath"
    , buildSystem = required "system"
    , buildLicense = optional "license"
    , buildHomepage = optional "homepage"
    , buildMaintainers = optional "maintainers"
    , buildMaxsilent = optional "maxsilent"
    , buildTimeout = optional "timeout"
    , buildIschannel = required "ischannel"
    , buildIscurrent = optional "iscurrent"
    , buildNixexprinput = optional "nixexprinput"
    , buildNixexprpath = optional "nixexprpath"
    , buildPriority = required "priority"
    , buildGlobalpriority = required "globalpriority"
    , buildStarttime = optional "starttime"
    , buildStoptime = optional "stoptime"
    , buildIscachedbuild = optional "iscachedbuild"
    , buildBuildstatus = optional "buildstatus"
    , buildSize = optional "size"
    , buildClosuresize = optional "closuresize"
    , buildReleasename = optional "releasename"
    , buildKeep = required "keep"
    }
  )

---- Types for table: buildstepoutputs ----

data Buildstepoutput' c1 c2 c3 c4 =
  Buildstepoutput
    { buildstepoutputBuild  :: c1
    , buildstepoutputStepnr :: c2
    , buildstepoutputName   :: c3
    , buildstepoutputPath   :: c4
    }

type Buildstepoutput = Buildstepoutput' Int32 Int32 Text Text

type BuildstepoutputReadColumns = Buildstepoutput' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type BuildstepoutputWriteColumns = Buildstepoutput' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type BuildstepoutputNullableColumns = Buildstepoutput' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildstepoutputNullable = Buildstepoutput' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableBuildstepoutput :: BuildstepoutputNullable -> Maybe Buildstepoutput
fromNullableBuildstepoutput = fromNullable

$(makeAdaptorAndInstance "pBuildstepoutput" ''Buildstepoutput')

buildstepoutputTable :: Table BuildstepoutputWriteColumns BuildstepoutputReadColumns
buildstepoutputTable = Table "buildstepoutputs" (pBuildstepoutput
  Buildstepoutput
    { buildstepoutputBuild = required "build"
    , buildstepoutputStepnr = required "stepnr"
    , buildstepoutputName = required "name"
    , buildstepoutputPath = required "path"
    }
  )

---- Types for table: buildsteps ----

data Buildstep' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 =
  Buildstep
    { buildstepBuild          :: c1
    , buildstepStepnr         :: c2
    , buildstepType           :: c3
    , buildstepDrvpath        :: c4
    , buildstepBusy           :: c5
    , buildstepStatus         :: c6
    , buildstepErrormsg       :: c7
    , buildstepStarttime      :: c8
    , buildstepStoptime       :: c9
    , buildstepMachine        :: c10
    , buildstepSystem         :: c11
    , buildstepPropagatedfrom :: c12
    , buildstepOverhead       :: c13
    }

type Buildstep = Buildstep' Int32 Int32 Int32 (Maybe Text) Int32 (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) Text (Maybe Text) (Maybe Int32) (Maybe Int32)

type BuildstepReadColumns = Buildstep' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column (Nullable PGText)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type BuildstepWriteColumns = Buildstep' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

type BuildstepNullableColumns = Buildstep' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type BuildstepNullable = Buildstep' (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32)

fromNullableBuildstep :: BuildstepNullable -> Maybe Buildstep
fromNullableBuildstep = fromNullable

$(makeAdaptorAndInstance "pBuildstep" ''Buildstep')

buildstepTable :: Table BuildstepWriteColumns BuildstepReadColumns
buildstepTable = Table "buildsteps" (pBuildstep
  Buildstep
    { buildstepBuild = required "build"
    , buildstepStepnr = required "stepnr"
    , buildstepType = required "type"
    , buildstepDrvpath = optional "drvpath"
    , buildstepBusy = required "busy"
    , buildstepStatus = optional "status"
    , buildstepErrormsg = optional "errormsg"
    , buildstepStarttime = optional "starttime"
    , buildstepStoptime = optional "stoptime"
    , buildstepMachine = required "machine"
    , buildstepSystem = optional "system"
    , buildstepPropagatedfrom = optional "propagatedfrom"
    , buildstepOverhead = optional "overhead"
    }
  )

---- Types for table: cachedbazaarinputs ----

data Cachedbazaarinput' c1 c2 c3 c4 =
  Cachedbazaarinput
    { cachedbazaarinputUri        :: c1
    , cachedbazaarinputRevision   :: c2
    , cachedbazaarinputSha256Hash :: c3
    , cachedbazaarinputStorepath  :: c4
    }

type Cachedbazaarinput = Cachedbazaarinput' Text Int32 Text Text

type CachedbazaarinputReadColumns = Cachedbazaarinput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedbazaarinputWriteColumns = Cachedbazaarinput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedbazaarinputNullableColumns = Cachedbazaarinput' (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedbazaarinputNullable = Cachedbazaarinput' (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableCachedbazaarinput :: CachedbazaarinputNullable -> Maybe Cachedbazaarinput
fromNullableCachedbazaarinput = fromNullable

$(makeAdaptorAndInstance "pCachedbazaarinput" ''Cachedbazaarinput')

cachedbazaarinputTable :: Table CachedbazaarinputWriteColumns CachedbazaarinputReadColumns
cachedbazaarinputTable = Table "cachedbazaarinputs" (pCachedbazaarinput
  Cachedbazaarinput
    { cachedbazaarinputUri = required "uri"
    , cachedbazaarinputRevision = required "revision"
    , cachedbazaarinputSha256Hash = required "sha256hash"
    , cachedbazaarinputStorepath = required "storepath"
    }
  )

---- Types for table: cachedcvsinputs ----

data Cachedcvsinput' c1 c2 c3 c4 c5 c6 =
  Cachedcvsinput
    { cachedcvsinputUri        :: c1
    , cachedcvsinputModule     :: c2
    , cachedcvsinputTimestamp  :: c3
    , cachedcvsinputLastseen   :: c4
    , cachedcvsinputSha256Hash :: c5
    , cachedcvsinputStorepath  :: c6
    }

type Cachedcvsinput = Cachedcvsinput' Text Text Int32 Int32 Text Text

type CachedcvsinputReadColumns = Cachedcvsinput' (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedcvsinputWriteColumns = Cachedcvsinput' (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedcvsinputNullableColumns = Cachedcvsinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedcvsinputNullable = Cachedcvsinput' (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableCachedcvsinput :: CachedcvsinputNullable -> Maybe Cachedcvsinput
fromNullableCachedcvsinput = fromNullable

$(makeAdaptorAndInstance "pCachedcvsinput" ''Cachedcvsinput')

cachedcvsinputTable :: Table CachedcvsinputWriteColumns CachedcvsinputReadColumns
cachedcvsinputTable = Table "cachedcvsinputs" (pCachedcvsinput
  Cachedcvsinput
    { cachedcvsinputUri = required "uri"
    , cachedcvsinputModule = required "module"
    , cachedcvsinputTimestamp = required "timestamp"
    , cachedcvsinputLastseen = required "lastseen"
    , cachedcvsinputSha256Hash = required "sha256hash"
    , cachedcvsinputStorepath = required "storepath"
    }
  )

---- Types for table: cacheddarcsinputs ----

data Cacheddarcsinput' c1 c2 c3 c4 c5 =
  Cacheddarcsinput
    { cacheddarcsinputUri        :: c1
    , cacheddarcsinputRevision   :: c2
    , cacheddarcsinputSha256Hash :: c3
    , cacheddarcsinputStorepath  :: c4
    , cacheddarcsinputRevcount   :: c5
    }

type Cacheddarcsinput = Cacheddarcsinput' Text Text Text Text Int32

type CacheddarcsinputReadColumns = Cacheddarcsinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type CacheddarcsinputWriteColumns = Cacheddarcsinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type CacheddarcsinputNullableColumns = Cacheddarcsinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type CacheddarcsinputNullable = Cacheddarcsinput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32)

fromNullableCacheddarcsinput :: CacheddarcsinputNullable -> Maybe Cacheddarcsinput
fromNullableCacheddarcsinput = fromNullable

$(makeAdaptorAndInstance "pCacheddarcsinput" ''Cacheddarcsinput')

cacheddarcsinputTable :: Table CacheddarcsinputWriteColumns CacheddarcsinputReadColumns
cacheddarcsinputTable = Table "cacheddarcsinputs" (pCacheddarcsinput
  Cacheddarcsinput
    { cacheddarcsinputUri = required "uri"
    , cacheddarcsinputRevision = required "revision"
    , cacheddarcsinputSha256Hash = required "sha256hash"
    , cacheddarcsinputStorepath = required "storepath"
    , cacheddarcsinputRevcount = required "revcount"
    }
  )

---- Types for table: cachedgitinputs ----

data Cachedgitinput' c1 c2 c3 c4 c5 =
  Cachedgitinput
    { cachedgitinputUri        :: c1
    , cachedgitinputBranch     :: c2
    , cachedgitinputRevision   :: c3
    , cachedgitinputSha256Hash :: c4
    , cachedgitinputStorepath  :: c5
    }

type Cachedgitinput = Cachedgitinput' Text Text Text Text Text

type CachedgitinputReadColumns = Cachedgitinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedgitinputWriteColumns = Cachedgitinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedgitinputNullableColumns = Cachedgitinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedgitinputNullable = Cachedgitinput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableCachedgitinput :: CachedgitinputNullable -> Maybe Cachedgitinput
fromNullableCachedgitinput = fromNullable

$(makeAdaptorAndInstance "pCachedgitinput" ''Cachedgitinput')

cachedgitinputTable :: Table CachedgitinputWriteColumns CachedgitinputReadColumns
cachedgitinputTable = Table "cachedgitinputs" (pCachedgitinput
  Cachedgitinput
    { cachedgitinputUri = required "uri"
    , cachedgitinputBranch = required "branch"
    , cachedgitinputRevision = required "revision"
    , cachedgitinputSha256Hash = required "sha256hash"
    , cachedgitinputStorepath = required "storepath"
    }
  )

---- Types for table: cachedhginputs ----

data Cachedhginput' c1 c2 c3 c4 c5 =
  Cachedhginput
    { cachedhginputUri        :: c1
    , cachedhginputBranch     :: c2
    , cachedhginputRevision   :: c3
    , cachedhginputSha256Hash :: c4
    , cachedhginputStorepath  :: c5
    }

type Cachedhginput = Cachedhginput' Text Text Text Text Text

type CachedhginputReadColumns = Cachedhginput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedhginputWriteColumns = Cachedhginput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedhginputNullableColumns = Cachedhginput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedhginputNullable = Cachedhginput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableCachedhginput :: CachedhginputNullable -> Maybe Cachedhginput
fromNullableCachedhginput = fromNullable

$(makeAdaptorAndInstance "pCachedhginput" ''Cachedhginput')

cachedhginputTable :: Table CachedhginputWriteColumns CachedhginputReadColumns
cachedhginputTable = Table "cachedhginputs" (pCachedhginput
  Cachedhginput
    { cachedhginputUri = required "uri"
    , cachedhginputBranch = required "branch"
    , cachedhginputRevision = required "revision"
    , cachedhginputSha256Hash = required "sha256hash"
    , cachedhginputStorepath = required "storepath"
    }
  )

---- Types for table: cachedpathinputs ----

data Cachedpathinput' c1 c2 c3 c4 c5 =
  Cachedpathinput
    { cachedpathinputSrcpath    :: c1
    , cachedpathinputTimestamp  :: c2
    , cachedpathinputLastseen   :: c3
    , cachedpathinputSha256Hash :: c4
    , cachedpathinputStorepath  :: c5
    }

type Cachedpathinput = Cachedpathinput' Text Int32 Int32 Text Text

type CachedpathinputReadColumns = Cachedpathinput' (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedpathinputWriteColumns = Cachedpathinput' (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedpathinputNullableColumns = Cachedpathinput' (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedpathinputNullable = Cachedpathinput' (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableCachedpathinput :: CachedpathinputNullable -> Maybe Cachedpathinput
fromNullableCachedpathinput = fromNullable

$(makeAdaptorAndInstance "pCachedpathinput" ''Cachedpathinput')

cachedpathinputTable :: Table CachedpathinputWriteColumns CachedpathinputReadColumns
cachedpathinputTable = Table "cachedpathinputs" (pCachedpathinput
  Cachedpathinput
    { cachedpathinputSrcpath = required "srcpath"
    , cachedpathinputTimestamp = required "timestamp"
    , cachedpathinputLastseen = required "lastseen"
    , cachedpathinputSha256Hash = required "sha256hash"
    , cachedpathinputStorepath = required "storepath"
    }
  )

---- Types for table: cachedsubversioninputs ----

data Cachedsubversioninput' c1 c2 c3 c4 =
  Cachedsubversioninput
    { cachedsubversioninputUri        :: c1
    , cachedsubversioninputRevision   :: c2
    , cachedsubversioninputSha256Hash :: c3
    , cachedsubversioninputStorepath  :: c4
    }

type Cachedsubversioninput = Cachedsubversioninput' Text Int32 Text Text

type CachedsubversioninputReadColumns = Cachedsubversioninput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedsubversioninputWriteColumns = Cachedsubversioninput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedsubversioninputNullableColumns = Cachedsubversioninput' (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedsubversioninputNullable = Cachedsubversioninput' (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableCachedsubversioninput :: CachedsubversioninputNullable -> Maybe Cachedsubversioninput
fromNullableCachedsubversioninput = fromNullable

$(makeAdaptorAndInstance "pCachedsubversioninput" ''Cachedsubversioninput')

cachedsubversioninputTable :: Table CachedsubversioninputWriteColumns CachedsubversioninputReadColumns
cachedsubversioninputTable = Table "cachedsubversioninputs" (pCachedsubversioninput
  Cachedsubversioninput
    { cachedsubversioninputUri = required "uri"
    , cachedsubversioninputRevision = required "revision"
    , cachedsubversioninputSha256Hash = required "sha256hash"
    , cachedsubversioninputStorepath = required "storepath"
    }
  )

---- Types for table: failedpaths ----

data Failedpath' c1 =
  Failedpath
    { failedpathPath :: c1
    }

type Failedpath = Failedpath' Text

type FailedpathReadColumns = Failedpath' (Column PGText)

type FailedpathWriteColumns = Failedpath' (Column PGText)

type FailedpathNullableColumns = Failedpath' (Column (Nullable PGText))

type FailedpathNullable = Failedpath' (Maybe Text)

fromNullableFailedpath :: FailedpathNullable -> Maybe Failedpath
fromNullableFailedpath = fromNullable

$(makeAdaptorAndInstance "pFailedpath" ''Failedpath')

failedpathTable :: Table FailedpathWriteColumns FailedpathReadColumns
failedpathTable = Table "failedpaths" (pFailedpath
  Failedpath
    { failedpathPath = required "path"
    }
  )


---- Types for table: newsitems ----

data Newsitem' c1 c2 c3 c4 =
  Newsitem
    { newsitemId         :: c1
    , newsitemContents   :: c2
    , newsitemCreatetime :: c3
    , newsitemAuthor     :: c4
    }

type Newsitem = Newsitem' Int32 Text Int32 Text

type NewsitemReadColumns = Newsitem' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText)

type NewsitemWriteColumns = Newsitem' (Maybe (Column PGInt4)) (Column PGText) (Column PGInt4) (Column PGText)

type NewsitemNullableColumns = Newsitem' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type NewsitemNullable = Newsitem' (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Text)

fromNullableNewsitem :: NewsitemNullable -> Maybe Newsitem
fromNullableNewsitem = fromNullable

$(makeAdaptorAndInstance "pNewsitem" ''Newsitem')

newsitemTable :: Table NewsitemWriteColumns NewsitemReadColumns
newsitemTable = Table "newsitems" (pNewsitem
  Newsitem
    { newsitemId = optional "id"
    , newsitemContents = required "contents"
    , newsitemCreatetime = required "createtime"
    , newsitemAuthor = required "author"
    }
  )

---- Types for table: nrbuilds ----

data Nrbuild' c1 c2 =
  Nrbuild
    { nrbuildWhat  :: c1
    , nrbuildCount :: c2
    }

type Nrbuild = Nrbuild' Text Int32

type NrbuildReadColumns = Nrbuild' (Column PGText) (Column PGInt4)

type NrbuildWriteColumns = Nrbuild' (Column PGText) (Column PGInt4)

type NrbuildNullableColumns = Nrbuild' (Column (Nullable PGText)) (Column (Nullable PGInt4))

type NrbuildNullable = Nrbuild' (Maybe Text) (Maybe Int32)

fromNullableNrbuild :: NrbuildNullable -> Maybe Nrbuild
fromNullableNrbuild = fromNullable

$(makeAdaptorAndInstance "pNrbuild" ''Nrbuild')

nrbuildTable :: Table NrbuildWriteColumns NrbuildReadColumns
nrbuildTable = Table "nrbuilds" (pNrbuild
  Nrbuild
    { nrbuildWhat = required "what"
    , nrbuildCount = required "count"
    }
  )

---- Types for table: projectmembers ----

data Projectmember' c1 c2 =
  Projectmember
    { projectmemberProject  :: c1
    , projectmemberUsername :: c2
    }

type Projectmember = Projectmember' Text Text

type ProjectmemberReadColumns = Projectmember' (Column PGText) (Column PGText)

type ProjectmemberWriteColumns = Projectmember' (Column PGText) (Column PGText)

type ProjectmemberNullableColumns = Projectmember' (Column (Nullable PGText)) (Column (Nullable PGText))

type ProjectmemberNullable = Projectmember' (Maybe Text) (Maybe Text)

fromNullableProjectmember :: ProjectmemberNullable -> Maybe Projectmember
fromNullableProjectmember = fromNullable

$(makeAdaptorAndInstance "pProjectmember" ''Projectmember')

projectmemberTable :: Table ProjectmemberWriteColumns ProjectmemberReadColumns
projectmemberTable = Table "projectmembers" (pProjectmember
  Projectmember
    { projectmemberProject = required "project"
    , projectmemberUsername = required "username"
    }
  )

---- Types for table: projects ----

data Project' c1 c2 c3 c4 c5 c6 c7 =
  Project
    { projectName        :: c1
    , projectDisplayname :: c2
    , projectDescription :: c3
    , projectEnabled     :: c4
    , projectHidden      :: c5
    , projectOwner       :: c6
    , projectHomepage    :: c7
    }
  deriving(Generic)

type Project = Project' Text Text (Maybe Text) Int32 Int32 Text (Maybe Text)

instance ToJSON Project where
instance ElmType Project where

type ProjectReadColumns = Project' (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column PGInt4) (Column PGInt4) (Column PGText) (Column (Nullable PGText))

type ProjectWriteColumns = Project' (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGText)))

type ProjectNullableColumns = Project' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type ProjectNullable = Project' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableProject :: ProjectNullable -> Maybe Project
fromNullableProject = fromNullable

$(makeAdaptorAndInstance "pProject" ''Project')

projectTable :: Table ProjectWriteColumns ProjectReadColumns
projectTable = Table "projects" (pProject
  Project
    { projectName = required "name"
    , projectDisplayname = required "displayname"
    , projectDescription = optional "description"
    , projectEnabled = required "enabled"
    , projectHidden = required "hidden"
    , projectOwner = required "owner"
    , projectHomepage = optional "homepage"
    }
  )

---- Types for table: releasemembers ----

data Releasemember' c1 c2 c3 c4 =
  Releasemember
    { releasememberProject     :: c1
    , releasememberRelease     :: c2
    , releasememberBuild       :: c3
    , releasememberDescription :: c4
    }

type Releasemember = Releasemember' Text Text Int32 (Maybe Text)

type ReleasememberReadColumns = Releasemember' (Column PGText) (Column PGText) (Column PGInt4) (Column (Nullable PGText))

type ReleasememberWriteColumns = Releasemember' (Column PGText) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText)))

type ReleasememberNullableColumns = Releasemember' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type ReleasememberNullable = Releasemember' (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

fromNullableReleasemember :: ReleasememberNullable -> Maybe Releasemember
fromNullableReleasemember = fromNullable

$(makeAdaptorAndInstance "pReleasemember" ''Releasemember')

releasememberTable :: Table ReleasememberWriteColumns ReleasememberReadColumns
releasememberTable = Table "releasemembers" (pReleasemember
  Releasemember
    { releasememberProject = required "project"
    , releasememberRelease = required "release_"
    , releasememberBuild = required "build"
    , releasememberDescription = optional "description"
    }
  )

---- Types for table: releases ----

data Release' c1 c2 c3 c4 =
  Release
    { releaseProject     :: c1
    , releaseName        :: c2
    , releaseTimestamp   :: c3
    , releaseDescription :: c4
    }

type Release = Release' Text Text Int32 (Maybe Text)

type ReleaseReadColumns = Release' (Column PGText) (Column PGText) (Column PGInt4) (Column (Nullable PGText))

type ReleaseWriteColumns = Release' (Column PGText) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText)))

type ReleaseNullableColumns = Release' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type ReleaseNullable = Release' (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

fromNullableRelease :: ReleaseNullable -> Maybe Release
fromNullableRelease = fromNullable

$(makeAdaptorAndInstance "pRelease" ''Release')

releaseTable :: Table ReleaseWriteColumns ReleaseReadColumns
releaseTable = Table "releases" (pRelease
  Release
    { releaseProject = required "project"
    , releaseName = required "name"
    , releaseTimestamp = required "timestamp"
    , releaseDescription = optional "description"
    }
  )

---- Types for table: schemaversion ----

data Schemaversion' c1 =
  Schemaversion
    { schemaversionVersion :: c1
    }

type Schemaversion = Schemaversion' Int32

type SchemaversionReadColumns = Schemaversion' (Column PGInt4)

type SchemaversionWriteColumns = Schemaversion' (Column PGInt4)

type SchemaversionNullableColumns = Schemaversion' (Column (Nullable PGInt4))

type SchemaversionNullable = Schemaversion' (Maybe Int32)

fromNullableSchemaversion :: SchemaversionNullable -> Maybe Schemaversion
fromNullableSchemaversion = fromNullable

$(makeAdaptorAndInstance "pSchemaversion" ''Schemaversion')

schemaversionTable :: Table SchemaversionWriteColumns SchemaversionReadColumns
schemaversionTable = Table "schemaversion" (pSchemaversion
  Schemaversion
    { schemaversionVersion = required "version"
    }
  )

---- Types for table: starredjobs ----

data Starredjob' c1 c2 c3 c4 =
  Starredjob
    { starredjobUsername :: c1
    , starredjobProject  :: c2
    , starredjobJobset   :: c3
    , starredjobJob      :: c4
    }

type Starredjob = Starredjob' Text Text Text Text

type StarredjobReadColumns = Starredjob' (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type StarredjobWriteColumns = Starredjob' (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type StarredjobNullableColumns = Starredjob' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type StarredjobNullable = Starredjob' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableStarredjob :: StarredjobNullable -> Maybe Starredjob
fromNullableStarredjob = fromNullable

$(makeAdaptorAndInstance "pStarredjob" ''Starredjob')

starredjobTable :: Table StarredjobWriteColumns StarredjobReadColumns
starredjobTable = Table "starredjobs" (pStarredjob
  Starredjob
    { starredjobUsername = required "username"
    , starredjobProject = required "project"
    , starredjobJobset = required "jobset"
    , starredjobJob = required "job"
    }
  )

---- Types for table: systemstatus ----

data Systemstatus' c1 c2 =
  Systemstatus
    { systemstatusWhat   :: c1
    , systemstatusStatus :: c2
    }

type Systemstatus = Systemstatus' Text JSON.Value

type SystemstatusReadColumns = Systemstatus' (Column PGText) (Column PGJson)

type SystemstatusWriteColumns = Systemstatus' (Column PGText) (Column PGJson)

type SystemstatusNullableColumns = Systemstatus' (Column (Nullable PGText)) (Column (Nullable PGJson))

type SystemstatusNullable = Systemstatus' (Maybe Text) (Maybe JSON.Value)

fromNullableSystemstatus :: SystemstatusNullable -> Maybe Systemstatus
fromNullableSystemstatus = fromNullable

$(makeAdaptorAndInstance "pSystemstatus" ''Systemstatus')

systemstatusTable :: Table SystemstatusWriteColumns SystemstatusReadColumns
systemstatusTable = Table "systemstatus" (pSystemstatus
  Systemstatus
    { systemstatusWhat = required "what"
    , systemstatusStatus = required "status"
    }
  )

---- Types for table: systemtypes ----

data Systemtype' c1 c2 =
  Systemtype
    { systemtypeSystem        :: c1
    , systemtypeMaxconcurrent :: c2
    }

type Systemtype = Systemtype' Text Int32

type SystemtypeReadColumns = Systemtype' (Column PGText) (Column PGInt4)

type SystemtypeWriteColumns = Systemtype' (Column PGText) (Column PGInt4)

type SystemtypeNullableColumns = Systemtype' (Column (Nullable PGText)) (Column (Nullable PGInt4))

type SystemtypeNullable = Systemtype' (Maybe Text) (Maybe Int32)

fromNullableSystemtype :: SystemtypeNullable -> Maybe Systemtype
fromNullableSystemtype = fromNullable

$(makeAdaptorAndInstance "pSystemtype" ''Systemtype')

systemtypeTable :: Table SystemtypeWriteColumns SystemtypeReadColumns
systemtypeTable = Table "systemtypes" (pSystemtype
  Systemtype
    { systemtypeSystem = required "system"
    , systemtypeMaxconcurrent = required "maxconcurrent"
    }
  )

---- Types for table: urirevmapper ----

data Urirevmapper' c1 c2 =
  Urirevmapper
    { urirevmapperBaseuri :: c1
    , urirevmapperUri     :: c2
    }

type Urirevmapper = Urirevmapper' Text Text

type UrirevmapperReadColumns = Urirevmapper' (Column PGText) (Column PGText)

type UrirevmapperWriteColumns = Urirevmapper' (Column PGText) (Column PGText)

type UrirevmapperNullableColumns = Urirevmapper' (Column (Nullable PGText)) (Column (Nullable PGText))

type UrirevmapperNullable = Urirevmapper' (Maybe Text) (Maybe Text)

fromNullableUrirevmapper :: UrirevmapperNullable -> Maybe Urirevmapper
fromNullableUrirevmapper = fromNullable

$(makeAdaptorAndInstance "pUrirevmapper" ''Urirevmapper')

urirevmapperTable :: Table UrirevmapperWriteColumns UrirevmapperReadColumns
urirevmapperTable = Table "urirevmapper" (pUrirevmapper
  Urirevmapper
    { urirevmapperBaseuri = required "baseuri"
    , urirevmapperUri = required "uri"
    }
  )

---- Types for table: userroles ----

data Userrole' c1 c2 =
  Userrole
    { userroleUsername :: c1
    , userroleRole     :: c2
    }

type Userrole = Userrole' Text Text

type UserroleReadColumns = Userrole' (Column PGText) (Column PGText)

type UserroleWriteColumns = Userrole' (Column PGText) (Column PGText)

type UserroleNullableColumns = Userrole' (Column (Nullable PGText)) (Column (Nullable PGText))

type UserroleNullable = Userrole' (Maybe Text) (Maybe Text)

fromNullableUserrole :: UserroleNullable -> Maybe Userrole
fromNullableUserrole = fromNullable

$(makeAdaptorAndInstance "pUserrole" ''Userrole')

userroleTable :: Table UserroleWriteColumns UserroleReadColumns
userroleTable = Table "userroles" (pUserrole
  Userrole
    { userroleUsername = required "username"
    , userroleRole = required "role"
    }
  )

---- Types for table: users ----

data User' c1 c2 c3 c4 c5 c6 =
  User
    { userUsername     :: c1
    , userFullname     :: c2
    , userEmailaddress :: c3
    , userPassword     :: c4
    , userEmailonerror :: c5
    , userType         :: c6
    }

type User = User' Text (Maybe Text) Text Text Int32 Text

type UserReadColumns = User' (Column PGText) (Column (Nullable PGText)) (Column PGText) (Column PGText) (Column PGInt4) (Column PGText)

type UserWriteColumns = User' (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGText) (Column PGText) (Column PGInt4) (Column PGText)

type UserNullableColumns = User' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type UserNullable = User' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

fromNullableUser :: UserNullable -> Maybe User
fromNullableUser = fromNullable

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserWriteColumns UserReadColumns
userTable = Table "users" (pUser
  User
    { userUsername = required "username"
    , userFullname = optional "fullname"
    , userEmailaddress = required "emailaddress"
    , userPassword = required "password"
    , userEmailonerror = required "emailonerror"
    , userType = required "type"
    }
  )


----------------------------------------------------------------------------
-- not autogenerated

pgBuild :: Build -> BuildWriteColumns
pgBuild = pBuild Build
    { buildId             = const Nothing
    , buildFinished       = constant
    , buildTimestamp      = constant
    , buildProject        = constant
    , buildJobset         = pgStrictText
    , buildJob            = pgStrictText
    , buildNixname        = nullt
    , buildDescription    = nullt
    , buildDrvpath        = pgStrictText
    , buildSystem         = pgStrictText
    , buildLicense        = nullt
    , buildHomepage       = nullt
    , buildMaintainers    = nullt
    , buildMaxsilent      = null4
    , buildTimeout        = null4
    , buildIschannel      = constant
    , buildIscurrent      = null4
    , buildNixexprinput   = nullt
    , buildNixexprpath    = nullt
    , buildPriority       = constant
    , buildGlobalpriority = constant
    , buildStarttime      = null4
    , buildStoptime       = null4
    , buildIscachedbuild  = null4
    , buildBuildstatus    = null4
    , buildSize           = fmap (toNullable . pgInt8)
    , buildClosuresize    = fmap (toNullable . pgInt8)
    , buildReleasename    = nullt
    , buildKeep           = constant
    }
    where
      null4 = fmap (toNullable . pgInt4 . fromIntegral)
      nullt = fmap (toNullable . pgStrictText)

instance Default Constant Build BuildWriteColumns where
   def = Constant pgBuild
