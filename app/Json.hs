{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Json
  ( AbnormalDependencyS (..),
    SolvedDependencyS (..),
    SolvedPackageS (..),
    SysDepsS (..),
    ArchHSOutput (..),
    fromAbnormalDependency,
    fromSolvedDependency,
    fromSolvedPackage,
    fromFlag,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Distribution.ArchHs.FilesDB
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types

packageName :: PackageName -> Value
packageName (unPackageName -> name) = String $ T.pack name

archLinuxName :: ArchLinuxName -> Value
archLinuxName (ArchLinuxName name) = String $ T.pack name

flag :: PkgFlag -> Value
flag f =
  object
    [ "name" .= unFlagName (flagName f),
      "description" .= flagDescription f,
      "default" .= flagDefault f,
      "manual" .= flagManual f
    ]

toValueViaShow :: (Show a) => a -> Value
toValueViaShow = String . T.pack . show

data AbnormalDependencyS = AbnormalDependencyS
  { abPkgName :: PackageName,
    abDeps :: [PackageName]
  }

instance ToJSON AbnormalDependencyS where
  toJSON AbnormalDependencyS {..} =
    object
      [ "pkg" .= packageName abPkgName,
        "dep" .= listValue packageName abDeps
      ]

data SolvedDependencyS = SolvedDependencyS
  { sdPkgName :: PackageName,
    sdDepType :: [DependencyType],
    sdProvider :: Maybe DependencyProvider
  }

instance ToJSON SolvedDependencyS where
  toJSON SolvedDependencyS {..} =
    object
      [ "pkg" .= packageName sdPkgName,
        "dep_type" .= listValue toValueViaShow sdDepType,
        "provider" .= maybe Null toValueViaShow sdProvider
      ]

data SolvedPackageS = SolvedPackageS
  { spPkgName :: PackageName,
    spDeps :: [SolvedDependencyS],
    spProvider :: Maybe DependencyProvider
  }

instance ToJSON SolvedPackageS where
  toJSON SolvedPackageS {..} =
    object $
      [ "pkg" .= packageName spPkgName,
        "provider" .= maybe Null toValueViaShow spProvider
      ]
        ++ ["dep" .= listValue toJSON spDeps | null spProvider]

data SysDepsS = SysDepsS
  { ssFile :: File,
    ssArchPkg :: Maybe ArchLinuxName
  }

instance ToJSON SysDepsS where
  toJSON SysDepsS {..} =
    object
      [ "file" .= ssFile,
        "arch_pkg" .= case ssArchPkg of
          Just x -> archLinuxName x
          _ -> Null
      ]

data FlagsS = FlagsS
  { fPkg :: PackageName,
    fFlag :: [PkgFlag]
  }

instance ToJSON FlagsS where
  toJSON FlagsS {..} =
    object
      [ "pkg" .= packageName fPkg,
        "flag" .= listValue flag fFlag
      ]

data ArchHSOutput = ArchHSOutput
  { abnormal :: [AbnormalDependencyS],
    solved :: [SolvedPackageS],
    order :: [PackageName],
    sys :: [SysDepsS],
    flags :: [FlagsS]
  }

instance ToJSON ArchHSOutput where
  toJSON ArchHSOutput {..} =
    object
      [ "abnormal_dep" .= listValue toJSON abnormal,
        "solved_pkg" .= listValue toJSON solved,
        "recommended_order" .= listValue packageName order,
        "system_dep" .= listValue toJSON sys,
        "flags" .= listValue toJSON flags
      ]

fromAbnormalDependency :: (PackageName, [PackageName]) -> AbnormalDependencyS
fromAbnormalDependency (abPkgName, abDeps) = AbnormalDependencyS {..}

fromSolvedDependency :: SolvedDependency -> SolvedDependencyS
fromSolvedDependency SolvedDependency {..} = SolvedDependencyS _depName _depType _depProvider

fromSolvedPackage :: SolvedPackage -> SolvedPackageS
fromSolvedPackage ProvidedPackage {..} = SolvedPackageS _pkgName [] (Just _pkgProvider)
fromSolvedPackage SolvedPackage {..} = SolvedPackageS _pkgName (fromSolvedDependency <$> _pkgDeps) Nothing

fromFlag :: (PackageName, [PkgFlag]) -> FlagsS
fromFlag (fPkg, fFlag) = FlagsS {..}
