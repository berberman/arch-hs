{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Plan.Toolchain
  ( GHCReleases,
    Toolchain (..),
    loadGHCReleases,
    parseGHCReleases,
    stableGHCRelease,
    toolchainContains,
    toolchainArchPackages,
  )
where

import qualified Control.Exception as Exception
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Yaml as Yaml
import Distribution.ArchHs.Local (ghcLibList)
import Distribution.ArchHs.Name (isGHCLibs, toArchLinuxName)
import Distribution.ArchHs.Types (ArchLinuxName (..))
import Distribution.Parsec (simpleParsec)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version, versionNumbers)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

type GHCReleases = Map.Map Version (Map.Map PackageName Version)

data Toolchain = Toolchain
  { toolchainVersion :: Version,
    toolchainPackages :: Map.Map PackageName Version,
    toolchainInstalled :: Map.Map PackageName Version,
    toolchainTools :: Set.Set PackageName
  }

loadGHCReleases :: IO (Either String GHCReleases)
loadGHCReleases = do
  fetched <- Exception.try @HttpException $ do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/global-hints.yaml"
    httpLbs request manager
  pure $ case fetched of
    Left err -> Left $ "Unable to fetch upstream GHC metadata: " <> show err
    Right response
      | statusCode (responseStatus response) /= 200 -> Left $ "Unable to fetch upstream GHC metadata: " <> show (responseStatus response)
      | otherwise -> parseGHCReleases $ BL.toStrict $ responseBody response

parseGHCReleases :: BS.ByteString -> Either String GHCReleases
parseGHCReleases bytes = do
  raw <- either (Left . Yaml.prettyPrintParseException) Right $
    Yaml.decodeEither' @(Map.Map String (Map.Map String String)) bytes
  releases <- Map.fromList <$> traverse parseRelease (mapMaybe ghcRelease $ Map.toList raw)
  unless (not $ Map.null releases) $ Left "Upstream GHC metadata contains no compiler releases."
  pure releases
  where
    ghcRelease (compiler, packages) = (, packages) <$> stripPrefix "ghc-" compiler
    parseRelease (compiler, packages) = do
      release <- maybe (Left $ "Invalid GHC version: " <> compiler) Right $ simpleParsec compiler
      parsed <- Map.fromList <$> traverse parsePackage (Map.toList packages)
      unless (Map.lookup "ghc" parsed == Just release) $ Left $ "Missing or mismatched ghc version in upstream metadata for " <> compiler
      unless (all (`Map.member` parsed) ["base", "ghc-prim", "template-haskell"]) $
        Left $ "Incomplete bundled-library metadata for GHC " <> compiler
      pure (release, Map.delete "Win32" parsed)
    parsePackage (package, release) = do
      name <- maybe (Left $ "Invalid bundled package name: " <> package) Right $ simpleParsec package
      version <- maybe (Left $ "Invalid bundled version for " <> package <> ": " <> release) Right $ simpleParsec release
      pure (name, version)

stableGHCRelease :: Version -> Bool
stableGHCRelease version = case versionNumbers version of
  [_, minor, patch] -> even minor && patch > 0
  _ -> False

toolchainContains :: Toolchain -> PackageName -> Bool
toolchainContains toolchain name =
  isGHCLibs name || Map.member name (toolchainPackages toolchain) || Map.member name (toolchainInstalled toolchain) || Set.member name (toolchainTools toolchain)

toolchainArchPackages :: Toolchain -> Set.Set ArchLinuxName
toolchainArchPackages toolchain = Set.insert (ArchLinuxName "ghc") $ Set.fromList $ toArchLinuxName <$>
  (ghcLibList <> Map.keys (toolchainPackages toolchain) <> Map.keys (toolchainInstalled toolchain) <> Set.toList (toolchainTools toolchain))
