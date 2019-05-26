# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.2] - 2019-05-26

### Added

- Allow including the encoding when writing an XML document (#71)


## [0.3.1] - 2019-05-26

### Added

- Allow using either single or double quotes when writing an XML document (#70)


## [0.3.0] - 2018-10-30

### Added
- Quickly set the text content of an `Element` (#62)
- Replace all children of `Root` and `Element` (#62)
- Append multiple children to `Root` and `Element` (#62)
- Remove all children from `Root` and `Element` (#62)
- Remove individual children from `Root` and `Element` (#65)
- Extend DTD parsing to handle more internal DTDs (#50, #60)

### Changed

- The error type returned from parsing now implements `Error` and no longer exposes internal details (#57, #58)

### Fixed

- Fixes DTD parsing when a `/` is present. (#50, #60)
