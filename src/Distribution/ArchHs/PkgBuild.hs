{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Template of PKGBUILD file.
module Distribution.ArchHs.PkgBuild
  ( PkgBuild (..),
    mapLicense,
    showArchLicense,
    applyTemplate,
    felixTemplate,
  )
where

import qualified Data.Aeson as A
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Distribution.SPDX.LicenseId
import NeatInterpolation (text)
import qualified Web.ArchLinux.Types as Arch

-- | PkgBuild data type, representing needed information in filling the 'felixTemplate'.
data PkgBuild = PkgBuild
  { -- | Field @_hkgName@.
    _hkgName :: String,
    -- | Field @pkgname@
    _pkgName :: String,
    -- | Field @pkgver@
    _pkgVer :: String,
    -- | Field @pkgdesc@
    _pkgDesc :: String,
    -- | Field @url@
    _url :: String,
    -- | Field @license@
    _license :: String,
    -- | Array @depends@, which has been joined into 'String'
    _depends :: String,
    -- | Array @makedepends@, which has been joined into 'String'
    _makeDepends :: String,
    -- | Field @sha256sums@
    _sha256sums :: String,
    -- | License file name
    _licenseFile :: Maybe String,
    -- | Whether generate @prepare()@ bash function which calls @uusi@
    _enableUusi :: Bool,
    -- | Command-line flags
    _flags :: String
  }

-- | Map 'LicenseId' to 'ArchLicense'. License not provided by system will be mapped to @custom:...@.
mapLicense :: LicenseId -> Arch.License
mapLicense AGPL_3_0_only = Arch.AGPL3
mapLicense Apache_2_0 = Arch.Apache
mapLicense Artistic_2_0 = Arch.Artistic2_0
mapLicense CDDL_1_0 = Arch.CDDL
mapLicense CPL_1_0 = Arch.CPL
mapLicense EPL_1_0 = Arch.EPL
mapLicense GFDL_1_2_only = Arch.FDL1_2
mapLicense GFDL_1_3_only = Arch.FDL1_3
mapLicense GPL_2_0_only = Arch.GPL2
mapLicense GPL_3_0_only = Arch.GPL3
mapLicense LGPL_2_1_only = Arch.LGPL2_1
mapLicense LGPL_3_0_only = Arch.LGPL3
mapLicense LPPL_1_3c = Arch.LPPL
mapLicense MPL_1_0 = Arch.MPL
mapLicense MPL_2_0 = Arch.MPL2
mapLicense PHP_3_01 = Arch.PHP
mapLicense Python_2_0 = Arch.PSF
mapLicense Artistic_1_0_Perl = Arch.PerlArtistic
mapLicense Ruby = Arch.RUBY
mapLicense ZPL_2_1 = Arch.ZPL
mapLicense Unlicense = Arch.Unlicense
mapLicense W3C = Arch.W3C
mapLicense NullBSD = Arch.BSD
mapLicense BSD_1_Clause = Arch.BSD
mapLicense BSD_2_Clause = Arch.BSD
mapLicense BSD_3_Clause = Arch.BSD
mapLicense ISC = Arch.ISC
mapLicense MIT = Arch.MIT
mapLicense Zlib = Arch.ZLIB
mapLicense OFL_1_0 = Arch.OFL
mapLicense OFL_1_1 = Arch.OFL
mapLicense x = Arch.Custom . T.pack $ show x

-- | Show an archlinux license
showArchLicense :: Arch.License -> String
showArchLicense license = case A.toJSON license of
  (A.String x) -> T.unpack x
  _ -> error "impossible"

-- | Apply 'PkgBuild' to 'felixTemplate'.
applyTemplate :: PkgBuild -> String
applyTemplate PkgBuild {..} =
  unpack $
    felixTemplate
      (pack _hkgName)
      (pack _pkgName)
      (pack _pkgVer)
      (pack _pkgDesc)
      (pack _url)
      (pack _license)
      (pack _depends)
      (pack _makeDepends)
      (pack _sha256sums)
      ( case _licenseFile of
          Just n -> "\n" <> installLicense (pack n)
          _ -> "\n"
      )
      (if _enableUusi then "\n" <> uusi <> "\n\n" else "\n")
      ("\n" <> check <> "\n\n")
      ( pack $ case _flags of
          [] -> ""
          xs -> "\\\n" <> xs
      )

-- | Text of @check()@ function.
check :: Text
check =
  [text|
  check() {
    cd $$_hkgname-$$pkgver
    runhaskell Setup test
  }
|]

-- | Text of statements which install license.
installLicense :: Text -> Text
installLicense licenseFile =
  [text|
    install -D -m644 $licenseFile -t "$$pkgdir"/usr/share/licenses/$$pkgname/
    rm -f "$$pkgdir"/usr/share/doc/$$pkgname/$licenseFile
|]

uusi :: Text
uusi =
  [text|
  prepare() {
    uusi $$_hkgname-$$pkgver/$$_hkgname.cabal
  }
|]

-- | A fixed template of haskell package in archlinux. See <https://wiki.archlinux.org/index.php/Haskell_package_guidelines Haskell package guidelines> .
felixTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
felixTemplate hkgname pkgname pkgver pkgdesc url license depends makedepends sha256sums licenseF uusiF checkF flags =
  [text|
  # This file was generated by https://github.com/berberman/arch-hs, please check it manually.
  # Maintainer: Your Name <youremail@domain.com>

  _hkgname=$hkgname
  pkgname=$pkgname
  pkgver=$pkgver
  pkgrel=1
  pkgdesc="$pkgdesc"
  url="$url"
  license=("$license")
  arch=('x86_64')
  depends=('ghc-libs'$depends)
  makedepends=('ghc'$makedepends)
  source=("https://hackage.haskell.org/packages/archive/$$_hkgname/$$pkgver/$$_hkgname-$$pkgver.tar.gz")
  sha256sums=($sha256sums)
  $uusiF
  build() {
    cd $$_hkgname-$$pkgver

    runhaskell Setup configure -O --enable-shared --enable-executable-dynamic --disable-library-vanilla \
      --prefix=/usr --docdir=/usr/share/doc/$$pkgname --datasubdir=$$pkgname --enable-tests \
      --dynlibdir=/usr/lib --libsubdir=\$$compiler/site-local/\$$pkgid \
      --ghc-option=-optl-Wl\,-z\,relro\,-z\,now \
      --ghc-option='-pie' $flags

    runhaskell Setup build
    runhaskell Setup register --gen-script
    runhaskell Setup unregister --gen-script
    sed -i -r -e "s|ghc-pkg.*update[^ ]* |&'--force' |" register.sh
    sed -i -r -e "s|ghc-pkg.*unregister[^ ]* |&'--force' |" unregister.sh
  }
  $checkF
  package() {
    cd $$_hkgname-$$pkgver

    install -D -m744 register.sh "$$pkgdir"/usr/share/haskell/register/$$pkgname.sh
    install -D -m744 unregister.sh "$$pkgdir"/usr/share/haskell/unregister/$$pkgname.sh
    runhaskell Setup copy --destdir="$$pkgdir"$licenseF
  }
|]
