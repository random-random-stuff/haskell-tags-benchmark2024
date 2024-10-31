Change log
==========

### 2.1.0.5

* Remove deprecated time functions

### 1.0.0.3

* Switched to megaparsec, resulting in double the speed of the old parser.

### 1.0.0.1
* Improve docs

### 1.0.0.0
* Use `Vector` over `List` internally, as per discussion in [issue 13](https://github.com/cies/htoml/issues/13)

### 0.2.0.1
* Expose `ToJSON` implementation
* Remove unused .cabal dependency (thanks @tmcgilchrist)

#### 0.2.0.0
* Compatible with TOML 0.4.0
* Improve test suite (all test now pass -- thanks @HuwCampbell)
* Slight API breakage (therefore major version bump)
* Use Parsec's parser state to track explicitness of table definitions (thanks @HuwCampbell)
* Clean up docs and code

#### 0.1.0.3
* GHC 7.10 compatibility fix (thanks @erebe)
* Allow time >= 1.5.0, by using some CPP trickery
* Improve README based on
  [feedback on Reddit](http://www.reddit.com/r/haskell/comments/2s376c/show_rhaskell_htoml_a_parser_for_toml_files)

#### 0.1.0.2
* Update the REAMDE
* Add/relax dependency version contraints where applicable
* Fix all warnings
* Add `CHANGES.md`

#### 0.1.0.1
* Fix `cabal configure` error in cabal file

#### 0.1.0.0
* Initial upload to Hackage
