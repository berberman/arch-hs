# arch-hs

[![GitHub CI](https://github.com/berberman/arch-hs/workflows/CI/badge.svg)](https://github.com/berberman/arch-hs/actions)
[![Build Status](https://travis-ci.com/berberman/arch-hs.svg?branch=master)](https://travis-ci.com/berberman/arch-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A program generating PKGBUILD for hackage packages. Special thanks to [felixonmars](https://github.com/felixonmars/felixonmars).


## Introduction

Given a name of a package in hackage, this program can generate its corresponding PKGBUILD draft.
It has a naive built-in dependency solver, which can fetch all dependencies we need to produce a archlinux package. 
During the dependency calculation, all version constraints will be discarded due to the arch haskell packaging strategy,
and packages already exist in the [community](https://www.archlinux.org/packages/) will be excluded.

## Prerequisite

* Pacman database, i.e., archlinux system.

* Hackage database tarball, usually provided by `cabal-install`.

This program is not released currently, thus `stack` is required to build and run this from source.

## Usage

### Install from AUR

```bash
❯ yay -S arch-hs-git
```

### Examples:

```
❯ arch-hs -o "/home/berberman/Desktop/test/" termonad
```

This will generate a series of PKGBUILD including termonad with its dependencies into the output dir.

```
❯ arch-hs -f inline-c:gsl-example:true -o "/home/berberman/Desktop/test/" termonad
```

Using `-f` can pass flags, which may affect the results of solving.  
For all available options, have a look at the help message:

```
❯ arch-hs --help
arch-hs - a program generating PKGBUILD for hackage packages.

Usage: arch-hs [-h|--hackage PATH] [-o|--output PATH] 
               [-f|--flags package_name:flag_name:true|false,...] TARGET
  Try to reach the TARGET QAQ.

Available options:
  -h,--hackage PATH        Path to
                           00-index.tar (default: "~/.cabal/packages/YOUR_HACKAGE_MIRROR/00-index.tar")
  -o,--output PATH         Output path to generated PKGBUILD files (empty means
                           dry run)
  -f,--flags package_name:flag_name:true|false,...
                           Flag assignments for packages - e.g.
                           inline-c:gsl-example:true (separated by ',')
  -a,--aur                 Enable AUR searching.
  -h,--help                Show this help text

```

## Limitations

* The dependency solver will **only** expand the dependencies of *executables* and *libraries* recursively, because
circular dependency lies ubiquitously involving *test suites*, *benchmarks*, and their *buildTools*.

* Currently, this program's functionality is limited to dependency processing, whereas necessary procedures like
file patches, loose of version constraints, etc. are need to be done manually, so **DO NOT** give too much trust in generated PKGBUILD files.

## ToDoList

* Structuralized PKGBUILD template.

* ~~AUR support.~~ ✔

* Logging system.

* Ability to Switch *buildable* of a component in the beginning.