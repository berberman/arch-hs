# Changelog

`arch-hs` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.11.0.0

- Add `arch-hs-rdepcheck`

- Support Cabal 3.6

- Fix datasubdir generation

- Update dependency versions

- Update name preset

## 0.10.2.0

- Don't treat special licenses as custom licenses

- Add `--datasubdir` to template

## 0.10.1.0

- Always generate `-f` or `-f-` for each flag in PKGBUILD

- Better prompt for missing packages

- Fix the bug that resolved targets were not filtered out correctly when processing sub-libraries

- Update name preset

## 0.10.0.0

- Add `arch-hs-sync list` to list Haskell packages in [community]

- Remove generating meta package

- Add `--install-deps` to call pacman to install all dependencies of a target

- Fix the list of packages to be packed is not consistent

- Add `--no-skip-missing` to consider abnormal dependencies in packaging

- Update dependency versions

- Update name preset

## 0.9.1.0

- Support Cabal 3.4

- Use the version of [`haskell-ghc`](https://archlinux.org/packages/community/x86_64/ghc/) to evaluate condition trees

- Fix the bug that sub-lib components appear wrongly in results

## 0.9.0.0

- Remove `arch-hs-submit` executable

- Add `arch-hs-sync` executable, which has two commands:

  - `arch-hs-sync submit` - the same as `arch-hs-submit` previously
  - `arch-hs-sync check` - check version inconsistencies between [community] and Hackage

- Improve CLI options:

  - values of `--skip` and `--flag` are no longer separated by comma
  - the options can be set many times, for example: `arch-hs -f packageA:flagA:true -f packageB:flagB:false`

- Update name preset

## 0.8.0.0

- Fix the bug that unable to run with dependency-free packages

- Do not generate PKGBUILDs for missing dependencies of the target

- Support resolving sub-libraries in `arch-hs-diff`

- Remove `falseList`, which `arch-hs-submit` no longer uses

## 0.7.1.0

- Bump ghc version to 8.10.4

- Fix some output formats

## 0.7.0.0

- Support dumping output of `arch-hs` to JSON

- Support resolving setup dependencies in `arch-hs-diff`

- Fix wrong concatenation direction of differentiating dependencies in `arch-hs-diff`

- Replace `req` with [`arch-web`](https://github.com/berberman/arch-web) and `http-client`

- Make `getLatestSHA256` become total function

## 0.6.2.0

- Use `Doc` to print prompt messages

- Fix wrong line feeds in `arch-hs-diff`

## 0.6.1.0

- Show dependens and makedepends in `arch-hs-diff` even if there is no difference between two target versions

- Bump ghc version to 8.10.3

- Fix an alignment error in `arch-hs-submit`

## 0.6.0.0

- Adopt [prettyprinter](https://hackage.haskell.org/package/prettyprinter)

- Support resolving `pkgconfigDepends` and `extraLibs` using pacman files db

- Support resolving custom-setup dependencies

- Add an option to force `arch-hs` to run even if the target is in [community]

- Adopt [Diff](https://hackage.haskell.org/package/Diff) in `arch-hs-diff` and `arch-hs-submit`

- Fix the bug that `ghc` is missing in depends

- Fix typo ([#35](https://github.com/berberman/arch-hs/pull/35))

- Update dependencies ([#34](https://github.com/berberman/arch-hs/pull/34))

## 0.5.0.0

- Add `TargetDisappearException` to complain if target is clearly not reachable

- Warn abnormal dependencies

- Clear trace file if is not empty

- Collect test dependencies recursively

- Drop meaningless `alpm_errno_t`

- Update name preset ([#30](https://github.com/berberman/arch-hs/pull/30) [#32](https://github.com/berberman/arch-hs/pull/32))

- Fix a typo ([#29](https://github.com/berberman/arch-hs/pull/29))

## 0.4.0.0

- [Alpm](https://www.archlinux.org/pacman/libalpm.3.html) support

- Fix sub-libraries handling ([#22](https://github.com/berberman/arch-hs/issues/22))

- Fix missing build tools ([#24](https://github.com/berberman/arch-hs/issues/24))

- Fix flag comparison in diff ([#25](https://github.com/berberman/arch-hs/issues/25))

- Fix pretty printing of flags

- Update name preset ([#26](https://github.com/berberman/arch-hs/pull/26))

## 0.3.0.0

- Update name preset

- Add check in submit

## 0.2.0.0

- More accurate naming conversion between hackage representation and archlinux representation, according to [NAME_PRESET.json](https://github.com/berberman/arch-hs/blob/master/data/NAME_PRESET.json)

- Clearer project structure

- More reasonable exceptions

- Provide versions of haskell packages in archlinux community

- Add `arch-hs-submit` executable

## 0.1.1.0

- Add uusi option

## 0.1.0.0

- Support sub-libraries ([#16](https://github.com/berberman/arch-hs/issues/16))

- Split `arch-hs-uusi` into [`uusi`](https://github.com/berberman/uusi)

- Better performance in dependency resolving

- Fix recommended package order

- Trace for dependency resolving

- More reasonable PKGBUILD template ([#17](https://github.com/berberman/arch-hs/issues/16) [#18](https://github.com/berberman/arch-hs/issues/16) [#19](https://github.com/berberman/arch-hs/issues/16) [#20](https://github.com/berberman/arch-hs/issues/16))

- Fix indentation of flags' pretty printing

## 0.0.0.0

- Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/berberman/arch-hs/releases
