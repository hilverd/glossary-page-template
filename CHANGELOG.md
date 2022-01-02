# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Make textareas grow/shrink in height [automatically](https://css-tricks.com/the-cleanest-trick-for-autogrowing-textareas/).

### Changed
### Fixed
### Removed

- Remove unused `data-default-layout` attribute for now.

## [0.0.4] - 2022-01-01

### Changed

- Only allow listing the first term of an item as a related term. This should encourage people to list the "primary" term first and reduce unnecessary choices.
- Make it easier to release styling changes without having to update the HTML template. Most CSS classes in the template have now been moved to the CSS file.

## [0.0.3] - 2021-12-31

### Added

- Add `meta name="generator"` tag pointing to GitHub project page.
- Add instructions for building from sources to README.

### Fixed

- In dark mode, make cards a bit lighter than background for consistency with non-dark mode.
- Improve instructions for making changes. These were implying that the name of the HTML file doesn't matter, but at the moment it is expected to be `glossary.html`.
- Make `bin/prepare-release` not fail if `release/` already exists.
- Sort items in case-insensitive order (TOC and main list).
- Fix formatting error when JavaScript is disabled (a space would appear before the commas that separate related items).

## [0.0.2] - 2021-12-29

### Added

- Give focus to newly created term input fields.
- Give focus to newly created description details fields.

### Changed

- Disable autocompletion for term input fields.
- Say "See" instead of "See also" if item has no details.

### Fixed

- Bug fix: escape special chars when saving HTML.
- Correct release badge URL.

## [0.0.1] - 2021-12-28

### Added

- Initial release.

[Unreleased]: https://github.com/hilverd/glossary-page-template/compare/v0.0.4...HEAD
[0.0.4]: https://github.com/hilverd/glossary-page-template/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/hilverd/glossary-page-template/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/hilverd/glossary-page-template/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/hilverd/glossary-page-template/releases/tag/v0.0.1
