# arch-hs

[![GitHub CI](https://github.com/berberman/arch-hs/workflows/CI/badge.svg)](https://github.com/berberman/arch-hs/actions)
[![Build Status](https://travis-ci.com/berberman/arch-hs.svg?branch=master)](https://travis-ci.com/berberman/arch-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A program converting packges from hackage to arch PKGBUILD. Special thanks to [felixonmars](https://github.com/felixonmars/felixonmars).


## Introduction

Given a name of a package in hackage, this program can generate its corresponding PKGBUILD draft.
It has a naive built-in dependency solver, which can fetch all dependencies we need to produce a archlinux package. 
During the dependency calculation, all version constraints will be discarded due to the arch haskell packaging strategy, and packages already exist in the community will be excluded.

## Prerequisite

* Pacman database, i.e., archlinux system.

* Hackage database tarball, usually provided by cabal.

This program is not released currently, stack is required to build and run this from source.

## Usage

### Download the source code:
```bash
git clone https://github.com/berberman/arch-hs
cd arch-hs
```

### Build and install:
```bash
stack build
stack install
```

### Examples:

```bash
arch-hs -o "/home/berberman/Desktop/test/" termonad
```

This will generate a series of PKGBUILD including termonad with its dependencies into the output dir.

```bash
arch-hs -f "[(\"inline-c\",\"gsl-example\",True)]" -o "/home/berberman/Desktop/test/" termonad
```

Using `-f` can pass flags, which may affect the results of solving.
For the full usage of it, have a look at the help message:

```bash
arch-hs --help
Usage: arch-hs [-h|--hackage PATH] [-o|--output PATH] 
               [-f|--flags [("package_name","flag_name",True|False),...]] TARGET
  Try to reach the TARGET QAQ.
```

## Limitations

* The dependency solver will **only** expand the dependencies of *executables* and *libraries* recursively, because
circular dependency lies ubiquitously involving *test suites*, *benchmarks*, and their *buildTools*.

* Currently, this program's functionality is limited to dependency processing, whereas necessary procedures like
file patches, loose of version constraints, etc. are need to be done manually, so **DO NOT** give too much trust in generated PKGBUILD files.

## ToDoList

* Adjust PKGBUILD template dynamically.

* Aur support.

* Logging system.