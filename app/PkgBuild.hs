{-# LANGUAGE QuasiQuotes #-}

module PkgBuild where

import NeatInterpolation
import Data.Text

-- data PkgBuild = PkgBuild{
--   pkgName::Text,
--   pkgVer::Text,
--   pkgDesc::Text,
--   license::Text,
--   depends::Text,
--   make
-- }

felixTemplate :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
felixTemplate pkgname pkgver pkgdesc url license depends makedepends =
  [text|
  # Maintainer: Felix Yan <felixonmars@archlinux.org>
  # Contributor: berberman <hatsue@typed.icu>

  _hkgname=$pkgname
  pkgname=haskell-$pkgname
  pkgver=$pkgver
  pkgrel=1
  pkgdesc=$pkgdesc
  url=$url
  license=$license
  arch=('x86_64')
  depends=('ghc-lib' $depends)
  makedepends=('ghc' $makedepends)
  source=("https://hackage.haskell.org/packages/archive/$$_hkgname/$$pkgver/$$_hkgname-$$pkgver.tar.gz")
  sha256sums=('SKIP')

  prepare(){
    cd $$_hkgname-$$pkgver
  }

  build() {
    cd $$_hkgname-$$pkgver    

    runhaskell Setup configure -O --enable-shared --enable-executable-dynamic --disable-library-vanilla \
      --prefix=/usr --docdir=/usr/share/doc/$$pkgname --enable-tests \
      --dynlibdir=/usr/lib --libsubdir=\$$compiler/site-local/\$$pkgid \
      --ghc-option=-optl-Wl\,-z\,relro\,-z\,now \
      --ghc-option='-pie'

    runhaskell Setup build
    runhaskell Setup register --gen-script
    runhaskell Setup unregister --gen-script
    sed -i -r -e "s|ghc-pkg.*update[^ ]* |&'--force' |" register.sh
    sed -i -r -e "s|ghc-pkg.*unregister[^ ]* |&'--force' |" unregister.sh
  }

  check() {
    cd $$_hkgname-$$pkgver
    runhaskell Setup test
  }

  package() {
    cd $$_hkgname-$$pkgver

    install -D -m744 register.sh "$$pkgdir"/usr/share/haskell/register/$$pkgname.sh
    install -D -m744 unregister.sh "$$pkgdir"/usr/share/haskell/unregister/$$pkgname.sh
    runhaskell Setup copy --destdir="$$pkgdir"
    install -D -m644 "LICENSE" "$${pkgdir}/usr/share/licenses/$${pkgname}/LICENSE"
    rm -f "$${pkgdir}/usr/share/doc/$${pkgname}/LICENSE"
  } 
|]