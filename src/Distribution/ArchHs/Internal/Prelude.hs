module Distribution.ArchHs.Internal.Prelude(
  NFData,
  Generic,
  HasCallStack,
  when,
  (<|>),
  splitOn,
  stripPrefix,
  isPrefixOf,
  isInfixOf,
  groupBy,
  intercalate,
  nub,
  sort,
  sortBy,
  (\\),
  encodeUtf8,
  decodeUtf8,
  prettyShow,
  simpleParsec,
  (</>),
  module Lens.Micro,
  module Polysemy,
  module Polysemy.Error,
  module Polysemy.Reader,
  module Polysemy.State,
  module Polysemy.Trace,
  module Distribution.Types.PackageDescription,
  module Distribution.Types.GenericPackageDescription,
  module Distribution.Types.Version,
  module Distribution.Types.VersionRange,
  module Distribution.Types.PackageName,
  module Distribution.Types.UnqualComponentName,
  module Distribution.Types.Flag,
) where


import           Control.Applicative                          ((<|>))
import           Control.DeepSeq                              (NFData)
import           Control.Monad                                (when)
import           Data.List                                    (groupBy,
                                                               intercalate,
                                                               isInfixOf,
                                                               isPrefixOf, nub,
                                                               sort, sortBy,
                                                               stripPrefix,
                                                               (\\))
import           Data.List.Split                              (splitOn)
import           Data.Text.Encoding                           (decodeUtf8,
                                                               encodeUtf8)
import           Distribution.Parsec                          (simpleParsec)
import           Distribution.Pretty                          (prettyShow)
import           Distribution.Types.Flag
import           Distribution.Types.GenericPackageDescription
import           Distribution.Types.PackageDescription
import           Distribution.Types.PackageName
import           Distribution.Types.UnqualComponentName
import           Distribution.Types.Version
import           Distribution.Types.VersionRange
import           GHC.Generics                                 (Generic)
import           GHC.Stack                                    (HasCallStack)
import           Lens.Micro
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace
import           System.FilePath                              ((</>))
