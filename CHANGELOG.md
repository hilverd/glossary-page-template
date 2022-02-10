# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
### Changed
### Fixed

- Improve print layout.

### Removed

## [0.2.0] - 2022-02-09

### Added

- Add letter grid to sidebar for easier navigation.

## [0.1.0] - 2022-02-05

## Added

- Support custom file names via the `$FILE` environment variable.

### Changed

* Move index to sidebar and widen layout.

### Migrating from 0.0.8

Make the [relevant changes](https://github.com/hilverd/glossary-page-template/compare/v0.0.8...v0.1.0#diff-98bc2a2d2bb8717b67810a53ae6b5d1648649e912f6fe838d9f35ba770a9f506) to `glossary.html`.

## [0.0.8] - 2022-01-13

### Changed

- Scroll to relevant item after editing.

## [0.0.7] - 2022-01-10

### Added

- Allow customising `$HOST` when launching editor.

### Fixed

- Fix bug where `data-enable-help-for-making-changes` was always being set to true.
- Make `$PORT` actually work when launching editor.

### Migrating from 0.0.6

Make the [relevant changes](https://github.com/hilverd/glossary-page-template/compare/v0.0.6...v0.0.7#diff-98bc2a2d2bb8717b67810a53ae6b5d1648649e912f6fe838d9f35ba770a9f506) to `glossary.html`.

## [0.0.6] - 2022-01-05

### Changed

- Reduce width of "about" section for readability.

### Fixed

- Fix bug: abbreviations not correctly recognised from HTML source.
- Fix layout bug affecting the table of contents.

## [0.0.5] - 2022-01-02

### Added

- Make textareas grow/shrink in height [automatically](https://css-tricks.com/the-cleanest-trick-for-autogrowing-textareas/).

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

[Unreleased]: https://github.com/hilverd/glossary-page-template/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/hilverd/glossary-page-template/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/hilverd/glossary-page-template/compare/v0.0.8...v0.1.0
[0.0.8]: https://github.com/hilverd/glossary-page-template/compare/v0.0.7...v0.0.8
[0.0.7]: https://github.com/hilverd/glossary-page-template/compare/v0.0.6...v0.0.7
[0.0.6]: https://github.com/hilverd/glossary-page-template/compare/v0.0.5...v0.0.6
[0.0.5]: https://github.com/hilverd/glossary-page-template/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/hilverd/glossary-page-template/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/hilverd/glossary-page-template/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/hilverd/glossary-page-template/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/hilverd/glossary-page-template/releases/tag/v0.0.1
