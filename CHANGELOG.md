# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.4] - 2024-08-10

### Added

 - The `run` function, which `party` has been internally using to execute a parser in the most raw, detailed way.

## [1.0.3] - 2024-07-31

### Added

 - The `stateful_many` and `stateful_many1` combinators.

## [1.0.2] - 2024-04-05

### Added

 - 30ish tests.
 - More informative error messages for `not`.
 - The `until` combinator!

### Fixed

 - A bug in `try` that reported the wrong error position.

## [1.0.1] - 2024-03-28

### Fixed

 - Some `sep` and `sep1` bugs and poor performance.

### Changed

 - Relicensed to MPL-2.0 to be less viral. This is an increase in flexibility and in general projects that were depending on it under the GPLv3 license will not need to do anything to keep using it, as MPL-2.0 code can be used in GPLv3 codebases. Note, however, that I am not a lawyer and this should not be interpreted as legal advice.

## [1.0.0] - 2024-03-21

### Added

 - Parse errors return positions finally!
 - `digits` convenience function
 - `many_concat` convenience function
 - `many1_concat` convenience function
 - A little demo in the README

### Changed

 - Update dependencies for Gleam 1.0!
 - `alt` renamed to `either`.

### Fixed

 - Make pattern matching "exhaustive" with explicit panics for illegal inputs. This is required to make `party` compile with Gleam 1.0.