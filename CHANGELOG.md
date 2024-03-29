# Changelog

All notable changes to this project will be documented in this file.

## Unreleased changes

## 1.3.2

- Fix chain display when a lot of nodes are left behind in case of a protocol update.

## 1.3.1

- Remove finalization proof gas parameter from expected parameters since they
  are no longer present.

## [1.3.0]

- Remove blockSlot from expected response of block info, making the dashboard
  work with protocol version 6.

## [1.2.0]

## Fixed

- Display of genesis blocks in the block explorer.

## Changed

- Support updated middleware API.

## [1.1.0]

### Added

- Add ability to display the new smart contract "Upgraded" event.

## [1.0.10]

### Added

- Display the CCD/EUR exchange rate normalized to 1 EUR.
- Display the ConfigureBaker and ConfigureDelegation transactions
- Display pool, passive delegation and foundation block rewards
- Display transactions without associated events

### Fixed
- Correctly display the effective time of updates with immediate effect.
- Enabled special events paging in the block summary
- Corrected the node version column label in the node list

## [1.0.4]

- Add a parameter to ignore older node versions in statistics calculations.

## [1.0.3] - (2021-11-22)

### Added
- Validate transaction hash when using the transaction lookup page.

### Changed
- Rename GTU to CCD.
- Refer to encrypted transfer as shielded instead.

## [1.0.2] - (2021-10-04)

### Added
- Paging for transaction events in block explorer and the transaction lookup page.

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
