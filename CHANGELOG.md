# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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