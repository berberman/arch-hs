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
mapLicense AGPL_3_0_only = Arch.AGPL_3_0_only
mapLicense AGPL_3_0_or_later = Arch.AGPL_3_0_or_later
mapLicense Apache_2_0 = Arch.Apache_2_0
mapLicense Artistic_1_0_Perl = Arch.Artistic_1_0_Perl
mapLicense Artistic_2_0 = Arch.Artistic_2_0
mapLicense BSL_1_0 = Arch.BSL_1_0
mapLicense CC_BY_1_0 = Arch.CC_BY_1_0
mapLicense CC_BY_2_0 = Arch.CC_BY_2_0
mapLicense CC_BY_2_5 = Arch.CC_BY_2_5
mapLicense CC_BY_3_0_AT = Arch.CC_BY_3_0_AT
mapLicense CC_BY_3_0_US = Arch.CC_BY_3_0_US
mapLicense CC_BY_3_0 = Arch.CC_BY_3_0
mapLicense CC_BY_4_0 = Arch.CC_BY_4_0
mapLicense CC_BY_NC_1_0 = Arch.CC_BY_NC_1_0
mapLicense CC_BY_NC_2_0 = Arch.CC_BY_NC_2_0
mapLicense CC_BY_NC_2_5 = Arch.CC_BY_NC_2_5
mapLicense CC_BY_NC_3_0 = Arch.CC_BY_NC_3_0
mapLicense CC_BY_NC_4_0 = Arch.CC_BY_NC_4_0
mapLicense CC_BY_NC_ND_1_0 = Arch.CC_BY_NC_ND_1_0
mapLicense CC_BY_NC_ND_2_0 = Arch.CC_BY_NC_ND_2_0
mapLicense CC_BY_NC_ND_2_5 = Arch.CC_BY_NC_ND_2_5
mapLicense CC_BY_NC_ND_3_0_IGO = Arch.CC_BY_NC_ND_3_0_IGO
mapLicense CC_BY_NC_ND_3_0 = Arch.CC_BY_NC_ND_3_0
mapLicense CC_BY_NC_ND_4_0 = Arch.CC_BY_NC_ND_4_0
mapLicense CC_BY_NC_SA_1_0 = Arch.CC_BY_NC_SA_1_0
mapLicense CC_BY_NC_SA_2_0 = Arch.CC_BY_NC_SA_2_0
mapLicense CC_BY_NC_SA_2_5 = Arch.CC_BY_NC_SA_2_5
mapLicense CC_BY_NC_SA_3_0 = Arch.CC_BY_NC_SA_3_0
mapLicense CC_BY_NC_SA_4_0 = Arch.CC_BY_NC_SA_4_0
mapLicense CC_BY_ND_1_0 = Arch.CC_BY_ND_1_0
mapLicense CC_BY_ND_2_0 = Arch.CC_BY_ND_2_0
mapLicense CC_BY_ND_2_5 = Arch.CC_BY_ND_2_5
mapLicense CC_BY_ND_3_0 = Arch.CC_BY_ND_3_0
mapLicense CC_BY_ND_4_0 = Arch.CC_BY_ND_4_0
mapLicense CC_BY_SA_1_0 = Arch.CC_BY_SA_1_0
mapLicense CC_BY_SA_2_0_UK = Arch.CC_BY_SA_2_0_UK
mapLicense CC_BY_SA_2_1_JP = Arch.CC_BY_SA_2_1_JP
mapLicense CC_BY_SA_2_5 = Arch.CC_BY_SA_2_5
mapLicense CC_BY_SA_3_0_AT = Arch.CC_BY_SA_3_0_AT
mapLicense CC_BY_SA_3_0 = Arch.CC_BY_SA_3_0
mapLicense CC_BY_SA_4_0 = Arch.CC_BY_SA_4_0
mapLicense CC_PDDC = Arch.CC_PDDC
mapLicense CC0_1_0 = Arch.CC0_1_0
mapLicense CDDL_1_0 = Arch.CDDL_1_0
mapLicense CDDL_1_1 = Arch.CDDL_1_1
mapLicense CPL_1_0 = Arch.CPL_1_0
mapLicense EPL_1_0 = Arch.EPL_1_0
mapLicense EPL_2_0 = Arch.EPL_2_0
mapLicense FSFAP = Arch.FSFAP
mapLicense GFDL_1_1_invariants_only = Arch.GFDL_1_1_invariants_only
mapLicense GFDL_1_1_invariants_or_later = Arch.GFDL_1_1_invariants_or_later
mapLicense GFDL_1_1_no_invariants_only = Arch.GFDL_1_1_no_invariants_only
mapLicense GFDL_1_1_no_invariants_or_later = Arch.GFDL_1_1_no_invariants_or_later
mapLicense GFDL_1_1_only = Arch.GFDL_1_1_only
mapLicense GFDL_1_1_or_later = Arch.GFDL_1_1_or_later
mapLicense GFDL_1_2_invariants_only = Arch.GFDL_1_2_invariants_only
mapLicense GFDL_1_2_invariants_or_later = Arch.GFDL_1_2_invariants_or_later
mapLicense GFDL_1_2_no_invariants_only = Arch.GFDL_1_2_no_invariants_only
mapLicense GFDL_1_2_no_invariants_or_later = Arch.GFDL_1_2_no_invariants_or_later
mapLicense GFDL_1_2_only = Arch.GFDL_1_2_only
mapLicense GFDL_1_2_or_later = Arch.GFDL_1_2_or_later
mapLicense GFDL_1_3_invariants_only = Arch.GFDL_1_3_invariants_only
mapLicense GFDL_1_3_invariants_or_later = Arch.GFDL_1_3_invariants_or_later
mapLicense GFDL_1_3_no_invariants_only = Arch.GFDL_1_3_no_invariants_only
mapLicense GFDL_1_3_no_invariants_or_later = Arch.GFDL_1_3_no_invariants_or_later
mapLicense GFDL_1_3_only = Arch.GFDL_1_3_only
mapLicense GFDL_1_3_or_later = Arch.GFDL_1_3_or_later
mapLicense GPL_1_0_only = Arch.GPL_1_0_only
mapLicense GPL_1_0_or_later = Arch.GPL_1_0_or_later
mapLicense GPL_2_0_only = Arch.GPL_2_0_only
mapLicense GPL_2_0_or_later = Arch.GPL_2_0_or_later
mapLicense GPL_3_0_only = Arch.GPL_3_0_only
mapLicense GPL_3_0_or_later = Arch.GPL_3_0_or_later
mapLicense LGPL_2_0_only = Arch.LGPL_2_0_only
mapLicense LGPL_2_0_or_later = Arch.LGPL_2_0_or_later
mapLicense LGPL_2_1_only = Arch.LGPL_2_1_only
mapLicense LGPL_2_1_or_later = Arch.LGPL_2_1_or_later
mapLicense LGPL_3_0_only = Arch.LGPL_3_0_only
mapLicense LGPL_3_0_or_later = Arch.LGPL_3_0_or_later
mapLicense LGPLLR = Arch.LGPLLR
mapLicense LPL_1_0 = Arch.Custom "LPL-1.0"
mapLicense MPL_1_0 = Arch.MPL_1_0
mapLicense MPL_1_1 = Arch.MPL_1_1
mapLicense MPL_2_0 = Arch.MPL_2_0
mapLicense PHP_3_0 = Arch.PHP_3_0
mapLicense PHP_3_01 = Arch.PHP_3_01
mapLicense PSF_2_0 = Arch.PSF_2_0
mapLicense Ruby = Arch.Ruby
mapLicense Unlicense = Arch.Unlicense
mapLicense W3C = Arch.W3C
mapLicense WTFPL = Arch.WTFPL
mapLicense ZPL_1_1 = Arch.ZPL_1_1
mapLicense ZPL_2_0 = Arch.ZPL_2_0
mapLicense ZPL_2_1 = Arch.ZPL_2_1
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
