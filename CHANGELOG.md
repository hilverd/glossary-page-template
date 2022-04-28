# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As far as this project is concerned, a _breaking change_ is one that requires the user to change the HTML file manually.
For breaking changes, the major version is increased.
This applies from version 0.5.0 onwards, as some versions before that have broken this rule.

## [Unreleased]

### Added

- Add favicons.
- Point `<meta name="generator" ...>` to `https://glossary.template/page`.

### Changed
### Fixed
### Removed

## [1.2.4] - 2022-04-24

### Fixed

- Improve instructions for making changes.

## [1.2.3] - 2022-04-24

### Fixed

- Improve mechanism for preventing FOUC.

## [1.2.2] - 2022-04-24

### Changed

- Roll back v1.2.1's hacks to work around Webkit/Safari's lack of support for `scroll-margin-top`. These were causing too many other issues.

### Fixed

- When exporting to Markdown, use CRLF for line breaks to support Windows.

## [1.2.1] - 2022-04-23

### Added

- On Chrome and Firefox, make enter, escape and arrow keys work for dropdown menu.

### Changed

- The help message for making changes now tries to adapt to custom file names.

### Fixed

- Make highlighting anchors work better on Safari.
- Remove superfluous empty lines when exporting to Markdown.

## [1.2.0] - 2022-03-18

### Added

- Allow exporting as Markdown.

## [1.1.1] - 2022-03-14

### Fixed

- Don't allow listing related terms that are not primary.

## [1.1.0] - 2022-03-13

### Added

- Show suggestions for related (primary) terms. These are either terms that occur in a definition for the current term, or primary terms for items that list the current term as related.

## [1.0.0] - 2022-03-10

### Added

- Allow changing title and about section in editor.

### Changed

- Improve accessibility.
- Host fonts on `glstatic.net` along with the other static assets, don't use Google Fonts.

### Fixed

- Prevent HTML element ID clashes caused by user-provided terms.

### Migrating from 0.5.0

- Quite a few changes need to be made to `glossary.html`. It is probably easiest to follow the steps below.
    1. Download the `glossary.html` template from the [latest release](https://github.com/hilverd/glossary-page-template/releases/latest).
    2. Copy the entire `<dl>...</dl>` element from your existing glossary and replace the `<dl>...</dl>` element in the newest template.
    3. Change the title and about section using the built-in editor.

## [0.5.0] - 2022-02-20

### Added

- When adding a new term, try to guess whether it is an abbreviation (but let the user override this choice).
- Show warning if details field contains line breaks.

### Changed

- Hide "Order items" radio buttons when printing.
- Don't change scroll position after deleting an item.
- Trim whitespace around terms/details when creating/editing.
- Only show silcrow if JavaScript is enabled.

## [0.4.3] - 2022-02-19

### Changed

- Scroll to new items after creating them.
- When adding related term, auto focus on select menu.

### Fixed

- Improve form validation.
  - Prevent adding a term that already exists elsewhere.
  - Prevent having multiple terms with the same ID in a single form.

## [0.4.2] - 2022-02-18

### Fixed

- Fix bug that would occur when trying to edit/delete while items are ordered by most frequent.
- Scroll back to item after editing it.

## [0.4.1] - 2022-02-18

### Changed

- Speed up switching between ordering alphabetically and by frequency (at the expense of keeping more data in memory).

## [0.4.0] - 2022-02-15

### Added

- Allow sorting items either alphabetically or by most frequent first. An item is considered more frequent if its terms occur more often in other items.

## [0.3.0] - 2022-02-10

### Added

- Add anchor links next to each term.

### Fixed

- Improve print layout.

## [0.2.0] - 2022-02-09

### Added

- Add letter grid to sidebar for easier navigation.

## [0.1.0] - 2022-02-05

## Added

- Support custom file names via the `$FILE` environment variable.

### Changed

- Move index to sidebar and widen layout.

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

[Unreleased]: https://github.com/hilverd/glossary-page-template/compare/v1.2.4...HEAD
[1.2.4]: https://github.com/hilverd/glossary-page-template/compare/v1.2.3...v1.2.4
[1.2.3]: https://github.com/hilverd/glossary-page-template/compare/v1.2.2...v1.2.3
[1.2.2]: https://github.com/hilverd/glossary-page-template/compare/v1.2.1...v1.2.2
[1.2.1]: https://github.com/hilverd/glossary-page-template/compare/v1.2.0...v1.2.1
[1.2.0]: https://github.com/hilverd/glossary-page-template/compare/v1.1.1...v1.2.0
[1.1.1]: https://github.com/hilverd/glossary-page-template/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/hilverd/glossary-page-template/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/hilverd/glossary-page-template/compare/v0.5.0...v1.0.0
[0.5.0]: https://github.com/hilverd/glossary-page-template/compare/v0.4.3...v0.5.0
[0.4.3]: https://github.com/hilverd/glossary-page-template/compare/v0.4.2...v0.4.3
[0.4.2]: https://github.com/hilverd/glossary-page-template/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/hilverd/glossary-page-template/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/hilverd/glossary-page-template/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/hilverd/glossary-page-template/compare/v0.2.0...v0.3.0
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
