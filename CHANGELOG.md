# Changelog

`arch-hs` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.4.0.0

* [Alpm](https://www.archlinux.org/pacman/libalpm.3.html) support

* Fix sub-libraries handling ([#22](https://github.com/berberman/arch-hs/issues/22))

* Fix missing build tools ([#24](https://github.com/berberman/arch-hs/issues/24))

* Fix flag comparison in diff ([#25](https://github.com/berberman/arch-hs/issues/25))

* Fix pretty printing of flags

* Update name preset ([#26](https://github.com/berberman/arch-hs/issues/26))

## 0.3.0.0

* Update name preset

* Add check in submit

## 0.2.0.0

* More accurate naming conversion between hackage representation and archlinux representation, according to [NAME_PRESET.json](https://github.com/berberman/arch-hs/blob/master/data/NAME_PRESET.json)

* Clearer project structure

* More reasonable exceptions

* Provide versions of haskell packages in archlinux community

* Add `arch-hs-submit` executable

## 0.1.1.0

* Add uusi option

## 0.1.0.0

* Support sub-libraries ([#16](https://github.com/berberman/arch-hs/issues/16))

* Split `arch-hs-uusi` into [`uusi`](https://github.com/berberman/uusi)

* Better performance in dependency resolving

* Fix recommended package order

* Trace for dependency resolving

* More reasonable PKGBUILD template ([#17](https://github.com/berberman/arch-hs/issues/16) [#18](https://github.com/berberman/arch-hs/issues/16) [#19](https://github.com/berberman/arch-hs/issues/16) [#20](https://github.com/berberman/arch-hs/issues/16))

* Fix indentation of flags' pretty printing

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/berberman/arch-hs/releases
