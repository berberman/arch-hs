# arch-hs

[![Hackage](https://img.shields.io/hackage/v/arch-hs.svg?logo=haskell)](https://hackage.haskell.org/package/arch-hs)
[![Dependency](https://img.shields.io/hackage-deps/v/arch-hs)](https://packdeps.haskellers.com/feed?needle=arch-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

| Env              | CI                                                                                                                        |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------- |
| pacman (-f alpm) | [![ArchLinux](https://github.com/berberman/arch-hs/actions/workflows/archlinux.yml/badge.svg)](https://github.com/berberman/arch-hs/actions/workflows/archlinux.yml) |
| cabal-install    | [![CI](https://github.com/berberman/arch-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/berberman/arch-hs/actions/workflows/ci.yml)     |

A program generating PKGBUILD for hackage packages. Special thanks to [felixonmars](https://github.com/felixonmars/).

**Notice that `arch-hs` will always support only the latest GHC version used by Arch Linux.**


## Introduction

Given the name of a package in hackage, `arch-hs` can generate PKGBUILD files, not only for the package
whose name is given, but also for all dependencies missing in [[community]](https://www.archlinux.org/packages/).
`arch-hs` has a naive built-in dependency solver, which can fetch those dependencies and find out which are required to be packaged.
During the dependency calculation, all version constraints will be discarded due to the arch haskell packaging strategy,
thus there is no guarantee of dependencies' version consistency.

## Prerequisite

`arch-hs` is a PKGBUILD text file generator, which is not integrated with `pacman`(See [Alpm Support](#Alpm-Support)), depending on nothing than:

* Pacman databases (`community.db`, `community.files`, `core.files`, and `extra.files`)

* Hackage index tarball (`01-index.tar`, or `00-index.tar` previously) -- usually provided by `cabal-install`

## Installation

`arch-hs` is portable, which means it's not restricted to Arch Linux.
However, `arch-hs` can use libalpm to load pacman database on Arch Linux,
and if you want to run on other systems, you have to build it from source.

### Install the latest release

```
# pacman -S arch-hs
```

`arch-hs` is available in [[community]](https://www.archlinux.org/packages/community/x86_64/arch-hs/), so you can install it using `pacman`.

### Install the development version

```
# pacman -S arch-hs-git
```

The `-git` version is available in [[archlinuxcn]](https://github.com/archlinuxcn/repo), following the latest git commit.

## Build

```
$ git clone https://github.com/berberman/arch-hs
```

Then build it via stack or cabal.

#### Stack
```
$ stack build
```

#### Cabal (dynamic)
```
$ cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic 
$ cabal build
```

## Usage

Just run `arch-hs` in command line with options and a target. Here is an example:
we will create the archlinux package of [gi-gdk](https://hackage.haskell.org/package/gi-gdk).

<details open>
<summary>
Output:
</summary>

```
$ arch-hs -o ~/test --alpm gi-gdk 
ⓘ Loading hackage from /home/berberman/.cabal/packages/hackage.haskell.org/01-index.tar
ⓘ Loading community.db from libalpm
ⓘ Start running...
ⓘ Solved:
...
gi-gdk                                      ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─gi-cairo (Lib, Setup)                    ✔ [community]
 ├─gi-gdkpixbuf (Lib, Setup)                ✘
 ├─gi-gio (Lib, Setup)                      ✘
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─gi-pango (Lib, Setup)                    ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
gi-gdkpixbuf                                ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─gi-gio (Lib, Setup)                      ✘
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
gi-gio                                      ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
gi-glib                                     ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
gi-gobject                                  ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
gi-harfbuzz                                 ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
gi-pango                                    ✘
 ├─Cabal (Setup)                            ✔ [community]
 ├─bytestring (Lib)                         ✔ [community]
 ├─containers (Lib)                         ✔ [community]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─gi-harfbuzz (Lib, Setup)                 ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [community]
 ├─haskell-gi-base (Lib)                    ✔ [community]
 ├─haskell-gi-overloading (Lib)             ✔ [community]
 ├─text (Lib)                               ✔ [community]
 └─transformers (Lib)                       ✔ [community]
...

ⓘ Recommended package order:
1. gi-glib
2. gi-gobject
3. gi-harfbuzz
4. gi-pango
5. gi-gio
6. gi-gdkpixbuf
7. gi-gdk

ⓘ Detected pkgconfig or extraLib from target(s):
gi-gdk:      gtk4.pc
gi-gdkpixbuf:gdk-pixbuf-2.0.pc
gi-gio:      gio-2.0.pc
gi-glib:     glib-2.0.pc
gi-gobject:  gobject-2.0.pc
gi-harfbuzz: harfbuzz.pc, harfbuzz-gobject.pc
gi-pango:    pango.pc

ⓘ Now finding corresponding system package(s) using files db:
ⓘ Loading [core] files from libalpm
ⓘ Loading [extra] files from libalpm
ⓘ Done:
gtk4.pc               ⇒   gtk4
gdk-pixbuf-2.0.pc     ⇒   gdk-pixbuf2
gio-2.0.pc            ⇒   glib2
glib-2.0.pc           ⇒   glib2
gobject-2.0.pc        ⇒   glib2
harfbuzz.pc           ⇒   harfbuzz
harfbuzz-gobject.pc   ⇒   harfbuzz
pango.pc              ⇒   pango

ⓘ Write file: /home/berberman/test/haskell-gi-gdk/PKGBUILD
ⓘ Write file: /home/berberman/test/haskell-gi-gdkpixbuf/PKGBUILD
ⓘ Write file: /home/berberman/test/haskell-gi-gio/PKGBUILD
ⓘ Write file: /home/berberman/test/haskell-gi-glib/PKGBUILD
ⓘ Write file: /home/berberman/test/haskell-gi-gobject/PKGBUILD
ⓘ Write file: /home/berberman/test/haskell-gi-harfbuzz/PKGBUILD
ⓘ Write file: /home/berberman/test/haskell-gi-pango/PKGBUILD
✔ Success!
```
</details>

This output tells us that in order to package `gi-gdk`, we must package its dependencies
listed in package order, which are not present in [community] repo. Particularly, `gi-gdk`
requires external system dependencies, so `arch-hs` can map them to system packages using files db.

```
$ tree ~/test
/home/berberman/test
├── haskell-gi-gdk
│   └── PKGBUILD
├── haskell-gi-gdkpixbuf
│   └── PKGBUILD
├── haskell-gi-gio
│   └── PKGBUILD
├── haskell-gi-glib
│   └── PKGBUILD
├── haskell-gi-gobject
│   └── PKGBUILD
├── haskell-gi-harfbuzz
│   └── PKGBUILD
└── haskell-gi-pango
    └── PKGBUILD
```

`arch-hs` generates PKGBUILD for each package. Let's see what we have in `./haskell-gi-harfbuzz/PKGBUILD`:

``` bash
# This file was generated by https://github.com/berberman/arch-hs, please check it manually.
# Maintainer: Your Name <youremail@domain.com>

_hkgname=gi-harfbuzz
pkgname=haskell-gi-harfbuzz
pkgver=0.0.3
pkgrel=1
pkgdesc="HarfBuzz bindings"
url="https://github.com/haskell-gi/haskell-gi"
license=("LGPL2.1")
arch=('x86_64')
depends=('ghc-libs' 'haskell-gi-glib' 'haskell-gi-gobject' 'haskell-gi' 'haskell-gi-base' 'haskell-gi-overloading' 'harfbuzz')
makedepends=('ghc')
source=("https://hackage.haskell.org/packages/archive/$_hkgname/$pkgver/$_hkgname-$pkgver.tar.gz")
sha256sums=('5f61c7b07427d0b77f867c3bc560043239c6184f98921295ce28fc8c9ce257e5')

build() {
  cd $_hkgname-$pkgver

  runhaskell Setup configure -O --enable-shared --enable-executable-dynamic --disable-library-vanilla \
    --prefix=/usr --docdir=/usr/share/doc/$pkgname --enable-tests \
    --dynlibdir=/usr/lib --libsubdir=\$compiler/site-local/\$pkgid \
    --ghc-option=-optl-Wl\,-z\,relro\,-z\,now \
    --ghc-option='-pie'

  runhaskell Setup build
  runhaskell Setup register --gen-script
  runhaskell Setup unregister --gen-script
  sed -i -r -e "s|ghc-pkg.*update[^ ]* |&'--force' |" register.sh
  sed -i -r -e "s|ghc-pkg.*unregister[^ ]* |&'--force' |" unregister.sh
}

package() {
  cd $_hkgname-$pkgver

  install -D -m744 register.sh "$pkgdir"/usr/share/haskell/register/$pkgname.sh
  install -D -m744 unregister.sh "$pkgdir"/usr/share/haskell/unregister/$pkgname.sh
  runhaskell Setup copy --destdir="$pkgdir"
  install -D -m644 LICENSE -t "$pkgdir"/usr/share/licenses/$pkgname/
  rm -f "$pkgdir"/usr/share/doc/$pkgname/LICENSE
}
```

`arch-hs` will collect the information from hackage db, and apply it into a fixed template after some processing steps
including renaming, matching license, and filling out dependencies etc.
However, packaging haven't been done so far.
`arch-hs` can't guarantee that this package can be built by ghc with the latest dependencies;
hence some patches may be required in `prepare()`, such as [uusi](#Uusi).


## Options

### Output

```
$ arch-hs -o ~/test TARGET
```

Using `-o` can generate a series of PKGBUILD including `TARGET` with its dependencies into the output dir. If you don't pass it, only dependency calculation will occur.

### Flag Assignments
```
$ arch-hs -f TARGET:FLAG_A:true TARGET
```

Using `-f` can pass flags, which may affect the results of resolving.  

### AUR Searching

```
$ arch-hs -a TARGET
```

With `-a`, `arch-hs` will regard AUR as another package provider, and it will try to search missing packages in AUR as well.

### Skipping Components

```
$ arch-hs -s COMPONENT_A TARGET
```

Using `-s` can force skip runnable components in dependency resolving.
This is useful when a package doesn't provide flag to disable its runnables, which will be built by default but are trivial in system level packaging.
Notice that this only makes sense in the lifetime of `arch-hs`, whereas generated PKGBUILD and actual build processes will not be affected.

### Extra Cabal Files

```
$ arch-hs -e ~/TARGET TARGET
```

**For Testing Purposes Only**

Using `-e` can include extra `.cabal` files as supplementary.
Useful when the `TARGET` hasn't been released to hackage.

### Trace

```
$ arch-hs --trace TARGET
```

With `--trace`, `arch-hs` can print the process of dependency resolving into stdout.

```
$ arch-hs --trace-file foo.log TARGET
```

Similar to `--trace`, but the log will be written into a file.

### Uusi

```
$ arch-hs -o ~/test --uusi TARGET
```

With `--uusi`, `arch-hs` will generate following snippet for each package:

```bash
prepare() {
  uusi $_hkgname-$pkgver/$_hkgname.cabal
}
```

See [uusi](https://hackage.haskell.org/package/uusi) for details.

### Alpm

See [Alpm Support](#Alpm-Support).

### Force

```
$ arch-hs --force TARGET
```

With `--force`, `arch-hs` will try to package even if the target is provided.

### Json

```
$ arch-hs --json ./output.json TARGET
```

With `--json`, `arch-hs` will dump information presented in stdout to file as JSON format, including:
  * abnormal dependencies
  * solved packages
  * recommended package order
  * system dependencies
  * flags

### No skip missing

```
$ arch-hs --no-skip-missing TARGET
```

With `--no-skip-missing`, `arch-hs` will try to package if the dependent of this package exist whereas this package does not.

## [Name preset](https://github.com/berberman/arch-hs/blob/master/data/NAME_PRESET.json)

To distribute a haskell package to archlinux, the name of package should be changed according to the naming convention:

* for haskell libraries, their names must have `haskell-` prefix

* for programs, it depends on circumstances

* names should always be in lower case

However, it's not enough to prefix the string with `haskell-` and transform to lower case; in some special situations, the hackage name
may have `haskell-` prefix already, or the case is irregular, thus we have to a name preset manually. Once a package distributed to archlinux,
whose name conform to above-mentioned situation, the name preset should be upgraded correspondingly.

## Diff

`arch-hs` also provides a component called `arch-hs-diff`. `arch-hs-diff` can show the differences of package description between two versions of a package,
and remind us if some required packages in community repo can't satisfy the version constraints, or they are non-existent.
This is useful in the subsequent maintenance of a package. For example:

```
$ arch-hs-diff --alpm comonad 5.0.6 5.0.7
ⓘ Loading community.db from libalpm
ⓘ Start running...
ⓘ Downloading cabal file from https://hackage.haskell.org/package/comonad-5.0.6/revision/0.cabal...
ⓘ Downloading cabal file from https://hackage.haskell.org/package/comonad-5.0.7/revision/0.cabal...
Package: comonad
Version: 5.0.6  ⇒  5.0.7
Synopsis: Comonads
URL: http://github.com/ekmett/comonad/
Depends:
  base  >=4 && <5
  containers  >=0.3 && <0.7
  distributive  >=0.2.2 && <1
  tagged  >=0.7 && <1
  transformers  >=0.2 && <0.6
  transformers-compat  >=0.3 && <1
--------------------------------------
  base  >=4 && <5
  containers  >=0.3 && <0.7
  distributive  >=0.2.2 && <1
  indexed-traversable  >=0.1 && <0.2
  tagged  >=0.7 && <1
  transformers  >=0.2 && <0.6
  transformers-compat  >=0.3 && <1
 
MakeDepends:
  base  -any
  doctest  >=0.11.1 && <0.17
--------------------------------------
  base  -any
  doctest  >=0.11.1 && <0.18
"doctest" is required to be in range (>=0.11.1 && <0.17), but [community] provides (0.17). 
Flags:
  comonad
    ⚐ test-doctests:
        description:
          
        default: True
        isManual: True
    ⚐ containers:
        description:
          You can disable the use of the `containers` package using `-f-containers`.

          Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.
        default: True
        isManual: True
    ⚐ distributive:
        description:
          You can disable the use of the `distributive` package using `-f-distributive`.

          Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.

          If disabled we will not supply instances of `Distributive`

        default: True
        isManual: True
--------------------------------------
  comonad
    ⚐ test-doctests:
        description:
          
        default: True
        isManual: True
    ⚐ containers:
        description:
          You can disable the use of the `containers` package using `-f-containers`.

          Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.
        default: True
        isManual: True
    ⚐ distributive:
        description:
          You can disable the use of the `distributive` package using `-f-distributive`.

          Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.

          If disabled we will not supply instances of `Distributive`

        default: True
        isManual: True
    ⚐ indexed-traversable:
        description:
          You can disable the use of the `indexed-traversable` package using `-f-indexed-traversable`.

          Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.

          If disabled we will not supply instances of `FunctorWithIndex`

        default: True
        isManual: True
✔ Success!
```

`arch-hs-diff` does not require hackage db, it downloads cabal files from hackage server instead. 

## Sync

For hackage distribution maintainers only, see `arch-hs-sync --help` for details.

## Limitations

* `arch-hs` will run into error, if solved targets contain cycle. Indeed, circular dependency lies ubiquitously in hackage because of tests,
but basic cycles are resolved manually in [community] by maintainers. So after the provider simplification, `arch-hs` can eliminate these cycles.
Nevertheless, if the target introduces new cycle or it dependens on a package in an unknown cycle, `arch-hs` will throw `CyclicExist` exception.

* `arch-hs` is not able to handle with complicated situations: the libraries of a package partially exist in hackage, some libraries include external sources, etc. 

* `arch-hs`'s functionality is limited to dependency processing, whereas necessary procedures like
file patches, version range processes, etc. They need to be done manually, so **DO NOT** give too much trust in generated PKGBUILD files.

## Alpm Support

[alpm](https://www.archlinux.org/pacman/libalpm.3.html) is Arch Linux Package Management library.
When running on Arch Linux, loading `community.db` and files dbs through this library is slightly faster than using the internal parser of `arch-hs`.
Thus, `arch-hs` provides a flag `alpm` to enable this feature:

```
cabal build -f alpm
```

This flag is enabled by default in `arch-hs` Arch Linux package.
Compiled with `alpm`, `arch-hs` uses alpm to load pacman databases by default.
In this case, the CLI flag `--no-alpm-community` and `--no-alpm-files` can be used to disable this feature.
> When `alpm` is enabled, `arch-hs` will lose the capability of specifying the path of `community.db` and directory of files db.


## Contributing

Issues and PRs are always welcome. **\_(:з」∠)\_**
