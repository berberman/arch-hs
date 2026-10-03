# arch-hs

[![Hackage](https://img.shields.io/hackage/v/arch-hs.svg?logo=haskell)](https://hackage.haskell.org/package/arch-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

| Env              | CI                                                                                                                        |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------- |
| pacman (-f alpm) | [![ArchLinux](https://github.com/berberman/arch-hs/actions/workflows/archlinux.yml/badge.svg)](https://github.com/berberman/arch-hs/actions/workflows/archlinux.yml) |
| cabal-install    | [![CI](https://github.com/berberman/arch-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/berberman/arch-hs/actions/workflows/ci.yml)     |

A program generating PKGBUILD for hackage packages. Special thanks to [felixonmars](https://github.com/felixonmars/).

**Notice that `arch-hs` will always support only the latest GHC version used by Arch Linux.**


## Introduction

Given the name of a package in hackage, `arch-hs` can generate PKGBUILD files, not only for the package
whose name is given, but also for all dependencies missing in [[extra]](https://www.archlinux.org/packages/).
`arch-hs` has a naive built-in dependency solver, which can fetch those dependencies and find out which are required to be packaged.
During the dependency calculation, all version constraints will be discarded due to the arch haskell packaging strategy,
thus there is no guarantee of dependencies' version consistency.

## Prerequisite

`arch-hs` is a PKGBUILD text file generator, which is not integrated with `pacman`(See [Alpm Support](#Alpm-Support)), depending on nothing than:

* Pacman databases (`extra.db`, `extra.files`, `core.db`, and `core.files`)

* Hackage index tarball (`01-index.tar`, or `00-index.tar` previously) -- usually provided by `cabal-install`

## Installation

`arch-hs` is portable, which means it's not restricted to Arch Linux.
However, `arch-hs` can optionally use libalpm to load pacman database on Arch Linux,
and if you want to run on other systems, you have to build it from source.

### Install the latest release

```
# pacman -S arch-hs
```

`arch-hs` is available in [[extra]](https://www.archlinux.org/packages/extra/x86_64/arch-hs/), so you can install it using `pacman`.

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
$ arch-hs -o ~/test gi-gdk
ⓘ Loading hackage from /home/berberman/.cabal/packages/hackage.haskell.org/01-index.tar
ⓘ Loading extra.db from /var/lib/pacman/sync/extra.db
ⓘ Start running...
ⓘ Solved:
...
gi-gdk                                      ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─gi-cairo (Lib, Setup)                    ✔ [extra]
 ├─gi-gdkpixbuf (Lib, Setup)                ✘
 ├─gi-gio (Lib, Setup)                      ✘
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─gi-pango (Lib, Setup)                    ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
gi-gdkpixbuf                                ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─gi-gio (Lib, Setup)                      ✘
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
gi-gio                                      ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
gi-glib                                     ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
gi-gobject                                  ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
gi-harfbuzz                                 ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
gi-pango                                    ✘
 ├─Cabal (Setup)                            ✔ [extra]
 ├─bytestring (Lib)                         ✔ [extra]
 ├─containers (Lib)                         ✔ [extra]
 ├─gi-glib (Lib, Setup)                     ✘
 ├─gi-gobject (Lib, Setup)                  ✘
 ├─gi-harfbuzz (Lib, Setup)                 ✘
 ├─haskell-gi (Lib, Setup)                  ✔ [extra]
 ├─haskell-gi-base (Lib)                    ✔ [extra]
 ├─haskell-gi-overloading (Lib)             ✔ [extra]
 ├─text (Lib)                               ✔ [extra]
 └─transformers (Lib)                       ✔ [extra]
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
ⓘ Loading [core] files from /var/lib/pacman/sync
ⓘ Loading [extra] files from /var/lib/pacman/sync
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
listed in package order, which are not present in [extra] repo. Particularly, `gi-gdk`
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
and remind us if some required packages in extra repo can't satisfy the version constraints, or they are non-existent.
By default, `arch-hs-diff` reads revision 0 cabal files from the local Hackage index.
`-h`/`--hackage` can be used to specify the index tarball path.
Use `--online` to download revision 0 cabal files from Hackage instead.
This is useful in the subsequent maintenance of a package. For example:

```
$ arch-hs-diff comonad 5.0.6 5.0.7
ⓘ Loading extra.db from /var/lib/pacman/sync/extra.db
ⓘ Loading hackage from /home/berberman/.cabal/packages/hackage.haskell.org/01-index.tar
ⓘ Start running...
ⓘ Reading revision 0 cabal file from /home/berberman/.cabal/packages/hackage.haskell.org/01-index.tar: comonad/5.0.6/comonad.cabal
ⓘ Reading revision 0 cabal file from /home/berberman/.cabal/packages/hackage.haskell.org/01-index.tar: comonad/5.0.7/comonad.cabal
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
"doctest" is required to be in range (>=0.11.1 && <0.17), but [extra] provides (0.17). 
Flags:
  comonad
    ⚐ test-doctests:
        description:
          
        default: True
        isManual: True
    ⚐ containers:
        description:
          You can disable the use of the `containers` package using `-f-containers`.

          Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.
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

          Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.
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

## Reverse dependency checks

`arch-hs-rdepcheck` lists Haskell reverse dependencies in [extra] and the Cabal version ranges they require:

```
$ arch-hs-rdepcheck aeson
Reverse dependency: agda
  Depends: >=1.1.2.0 && <2.3
...
```

For each reverse dependency's version in [extra], the command reads both the latest `.cabal` revision and revision 0 from the local Hackage index. When their dependency ranges differ, both are shown under `latest revision` and `revision 0`. Equivalent ranges are shown only once. This also works without a candidate version.

Pass an optional version to check whether that version satisfies every listed range. Ranges that accept the current [extra] version but reject the candidate are marked in red as `rdep`. Ranges that reject both versions are marked in yellow as `rdep-old`. Both are counted separately, and the command exits with a non-zero status only for newly unmet ranges (or runtime errors):

```
$ arch-hs-rdepcheck aeson 3.0
Reverse dependency: agda
  Depends: >=1.1.2.0 && <2.3
  rdep: 3.0 is outside Depends range (>=1.1.2.0 && <2.3)
Reverse dependency: haskell-example
  Depends: <2.2
  rdep-old: 3.0 is outside Depends range (<2.2)
...
Reverse dependency range check(s) failed: rdep=60, rdep-old=8
```

This example assumes the current [extra] version is 2.2.3.0. If only existing failures remain, the command prints a warning such as `Existing reverse dependency range failure(s): rdep=0, rdep-old=8` and exits successfully. Ranges satisfied by the candidate are not counted, even if they reject the current version.

Pass multiple targets to check them together. Each target can have its own optional candidate version:

```
$ arch-hs-rdepcheck aeson text
$ arch-hs-rdepcheck aeson 3.0 text 2.1
$ arch-hs-rdepcheck aeson 3.0 text
```

Results are combined by reverse dependency: each dependent package appears once, with the ranges and revision comparisons labeled by target. The final counts combine all targets, and newly unmet ranges for any target cause a non-zero exit status.

Candidate versions form one upgrade set. When a target depends on another target, its candidate version's Cabal metadata supplies the ranges, including newly added dependencies. Targets without a candidate version and other reverse dependencies keep their [extra] versions. Existing failures are identified using the installed dependent's ranges; revision comparisons use the corresponding revisions of the candidate and installed versions.

When revisions differ, each revision's counts accompany its ranges:

```
Reverse dependency: haskell-example
  latest revision (rdep=1, rdep-old=0):
    Depends: <3
    rdep: 3.0 is outside Depends range (<3)
  revision 0 (rdep=0, rdep-old=1):
    Depends: <2.2
    rdep-old: 3.0 is outside Depends range (<2.2)
```

The final totals and exit status use the latest revision; revision 0 is shown for comparison. If only one revision can be parsed, its ranges are still shown and the other revision is labeled `unchecked` with the lookup error.

## Planning coordinated updates

`arch-hs-plan` checks a proposed update set against both its Cabal dependencies and the reverse dependencies that stay in [extra]:

```
$ arch-hs-plan aeson 2.2.3.0 scientific 0.3.8.0
$ arch-hs-plan aeson scientific
```

Without `--solve`, explicit versions are checked exactly. When a version is omitted, the command selects the next newer preferred Hackage release. Every target uses its candidate metadata and the proposed versions of other targets. Packages outside the target set retain their installed versions. Library, executable, test, setup, and Haskell build-tool dependencies are included, using the installed GHC and the usual `-f PACKAGE:FLAG:true|false` assignments.

GHC can also be requested as a toolchain update:

```sh
$ arch-hs-plan ghc
$ arch-hs-plan --solve ghc
$ arch-hs-plan --solve ghc 9.8.1 aeson
```

For `ghc`, an omitted version selects the next stable upstream release after the repository compiler, not the latest release. Explicit versions are exact without `--solve` and minimums with it, just like other targets. The solver counts compiler release steps alongside package release steps when minimizing the update.

GHC plans fetch [Stackage's upstream GHC bundled-library snapshots](https://github.com/commercialhaskell/stackage-content/blob/master/stack/global-hints.yaml); no built Arch GHC package is needed. Each candidate compiler fixes its entire Unix library bundle, including newly bundled and removed libraries. Missing or invalid compiler metadata is an error rather than a reason to guess library versions. Bundled libraries cannot be requested or upgraded independently. The snapshots do not specify versions of compiler-provided executables such as `hsc2hs`; dependencies on these tools are explicitly reported as unchecked, rather than assuming the tools were removed or retaining their old versions.

The planner rechecks every repository Haskell package with the proposed compiler and bundled versions, including `impl(ghc ...)` conditionals. Existing-failure comparisons still use the repository compiler and dependency versions, so newly activated incompatibilities block the plan. With `--solve`, blocking packages can be updated automatically. Only changed bundled versions, including added or removed libraries, are shown separately; unchanged versions and empty bundled summaries are hidden. Bundled libraries do not become individual package updates in the commit message or rebuild command. GHC rebuild commands include `--ignore ghc-static` to override `genrebuild -H`'s default exclusion of `ghc`.

Add `--solve` to search for a compatible combination:

```
$ arch-hs-plan --solve aeson scientific
$ arch-hs-plan --solve aeson 2.2.3.0 scientific
```

In solve mode, explicit versions are minimums. The search starts at those minimums (or the next newer preferred release for an unversioned target) and advances through preferred releases in ascending order. When a dependency or reverse dependency blocks the plan, the solver can automatically add that package to the update set. It then checks the added package's dependencies and reverse dependencies, expanding recursively as needed. A missing Haskell dependency available on Hackage can also be added as a new package.

The solver minimizes the **total number of release steps beyond the starting set**. Adding a package at its next preferred release costs one step; each further release costs another. For a new package, its earliest preferred release costs one step. Alternative combinations keep their own update sets, so an added package does not force unrelated branches to update it. If one step for A avoids three steps for B, the one-step solution is chosen. Equal-cost solutions are ordered deterministically by package name and version. Explicit minimums may name deprecated versions; automatically selected versions respect Hackage's preferred-version ranges. No package is downgraded.

The output lists installed and proposed versions, marking automatically included packages with `(added by solver)`. It also shows the number of candidate sets checked and any blocking dependency or reverse dependency ranges. If no working set can be found, it reports the remaining conflicts and exits unsuccessfully.

Plans with version changes also print a commit message listing all updated packages and versions on one comma-separated line, including packages added by the solver. A copyable `genrebuild -H <pkgbases...>` command follows, using the corresponding Arch package bases of all packages in the plan and respecting package name presets. Blocked plans include both too, so their updates can be tried manually.

The planner compares the latest Cabal revision with revision 0 for the chosen packages and their reverse dependencies, showing only dependencies whose check outcomes change. Range-only differences are hidden when both revisions pass, block, warn, or remain unchecked; adding or removing an already-satisfied dependency is also hidden. Changes between those outcomes, including whether a revision can be checked, remain visible. Version selection and the final status use the latest revision; revision 0 is shown for comparison.

Existing repository incompatibilities are yellow `dep-old` or `rdep-old` warnings and do not block a plan. Missing metadata for existing reverse dependencies is reported as an unchecked warning. Newly introduced incompatibilities and missing or unparseable candidate metadata still block the plan.

For dependencies already used by the installed package in the same dependency category, candidate upper bounds already exceeded by the repository version are also warnings, even if the installed package's metadata omitted those bounds. This keeps incremental releases available instead of skipping ahead solely to accommodate an already newer dependency. New dependencies, newly unmet lower bounds, and dependency updates that newly cross an upper bound still block the plan. Warnings do not establish build compatibility.

The planner reads the latest revisions from the local Hackage index and accepts the usual `--extra` and `--hackage` paths. It does not build or install packages, validate non-Haskell system dependencies or ABI compatibility, or determine a build order. GHC and its bundled libraries remain fixed unless `ghc` is explicitly requested. Only GHC plans fetch upstream metadata; ordinary package plans remain local. A GHC plan checks ecosystem metadata compatibility, not compiler bootstrap requirements or Arch-specific changes to the upstream bundle. Refresh the local databases before planning against newer repository or Hackage metadata.

## Sync

For Hackage distribution maintainers, `arch-hs-sync check` compares Haskell package versions in [extra] with Hackage:

```
$ arch-hs-sync check
haskell-aeson in [extra] has version 2.2.3.0, but linked aeson in Hackage has newer versions 2.2.3.1, 2.2.3.2
```

Only non-deprecated Hackage versions newer than the [extra] version are reported. Version checks use Hackage index metadata, so they also report packages whose `.cabal` format is newer than the Cabal library used to build `arch-hs`.

Pass `--depcheck` to check whether each newer Hackage version is currently upgradable with the packages already in [extra]. A version is shown as `ok` only when both its dependency ranges are satisfied by [extra] and all current reverse dependency ranges accept that version:

```
$ arch-hs-sync check --depcheck
haskell-aeson in [extra] has version 2.2.3.0, but linked aeson in Hackage has newer versions 2.2.3.1 (existing: rdep-old=2), 2.2.3.2 (blocked: dep=1, rdep=1, rdep-old=2)
```

`rdep` counts ranges that accept the current [extra] version but reject the candidate. `rdep-old` counts ranges that reject both versions. Each failing range is counted separately, including different dependency sources of the same reverse dependency. Ranges satisfied by the candidate are not counted, even if they reject the current version.

Candidates with only existing reverse dependency failures are shown in yellow as `existing: rdep-old=N`. Candidates with direct dependency failures or newly unmet reverse dependency ranges are shown in red as `blocked`, with existing failures counted separately when present.

If a candidate's `.cabal` file cannot be parsed, `--depcheck` marks it as `unchecked: cabal parse failed` and continues checking the other candidates. Use `--verbose` to include the lookup error.

Add `--verbose` with `--depcheck` to list the dependency and reverse dependency ranges that fail for a version, with existing reverse dependency failures labeled `rdep-old:`:

```
$ arch-hs-sync check --depcheck --verbose
haskell-aeson in [extra] has version 2.2.3.0, but linked aeson in Hackage has newer versions 2.2.3.2 (blocked: dep=1, rdep=1, rdep-old=1)
  2.2.3.2:
    dep: scientific requires >=0.3 && <0.4, [extra] has 0.4
    rdep: haskell-example Depends requires >=2.2 && <2.2.3.2
    rdep-old: agda Depends requires >=1.1.2.0 && <2.2
```

Other sync commands, including `submit` and `list`, are documented in `arch-hs-sync --help`.

## Limitations

* `arch-hs` will run into error, if solved targets contain cycle. Indeed, circular dependency lies ubiquitously in hackage because of tests,
but basic cycles are resolved manually in [extra] by maintainers. So after the provider simplification, `arch-hs` can eliminate these cycles.
Nevertheless, if the target introduces new cycle or it dependens on a package in an unknown cycle, `arch-hs` will throw `CyclicExist` exception.

* `arch-hs` is not able to handle with complicated situations: the libraries of a package partially exist in hackage, some libraries include external sources, etc. 

* `arch-hs`'s functionality is limited to dependency processing, whereas necessary procedures like
file patches, version range processes, etc. They need to be done manually, so **DO NOT** give too much trust in generated PKGBUILD files.

## Alpm Support

[alpm](https://www.archlinux.org/pacman/libalpm.3.html) is Arch Linux Package Management library.
When running on Arch Linux, `arch-hs` can load `extra.db` and files dbs through this library instead of using its internal parser.
`arch-hs` provides a Cabal flag `alpm` to build this feature:

```
cabal build -f alpm
```

This flag is enabled by default in `arch-hs` Arch Linux package.
Compiled with `alpm`, `arch-hs` still reads pacman databases from `/var/lib/pacman/sync` by default.
Pass `--alpm` to load pacman databases through libalpm explicitly:

```
arch-hs --alpm -o ~/test gi-gdk
```

For commands that load only `extra.db`, such as `arch-hs-diff`, `arch-hs-sync`, `arch-hs-rdepcheck`, and `arch-hs-plan`, `--alpm` applies to `extra.db`.
For `arch-hs`, `--alpm` applies to both `extra.db` and files dbs.
When `--alpm` is used, explicit `--extra` and `--files` paths are ignored.


## Contributing

Issues and PRs are always welcome. **\_(:з」∠)\_**
