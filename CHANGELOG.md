# Changelog

All notable changes to this project will be documented in this file.

## Unreleased changes

### Added

### Changed
- Make GTU per Euro update in block explorer more readable.
- Remove dead link to account documentation.
- Attempt to parse RegisterData bytes as CBOR, similar to memo transactions.
- Show more readable error messages for http error codes.
- Display dates in local time and with UTC offset.

### Fixed
- Fixed text wrapping problem in the block explorer
- Fixed parsing memo byte string as CBOR. Will only succeed if consumes all of the bytes.
- Fixed NaN when displaying 0 percent.

## [1.0.1] - (2021-09-09)

### Added
- Expose package.json version in UI
- Google Analytics and a cookie consent banner
- Support for Memo transactions and events

### Changed
- Update chain visualization:
  - Move branches around for a more compact tree.
  - Always have the branch with the most popular best block on top.
  - Display blocks on discarded branches as greyed out.
  - Display an info icon with tooltip, explaining what the purple bars mean.

### Fixed
- Fix overflow issue, making some UI elements to display text vertically
- Fix incorrect parsing of key updates
- Make contract addresses copyable in the details view
- Fix text on minting GTU in the chain explorer

## [1.0.0] - (2021-05-20)
