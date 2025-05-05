# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As far as this project is concerned, a _breaking change_ is one that requires the user to change the HTML file manually, or that might break existing formatting.
For breaking changes, the major version is increased.
This applies from version 0.5.0 onwards, as some versions before that have broken this rule.

## [Unreleased]

### Added
### Changed

- Use comboboxes instead of `<select>` elements for selecting related items.
- Use comboboxes instead of checkboxes for adding tags to items.
- Limit search results to at most 10 and add a "Showing x of y matches" message.

### Fixed

- Fix opacity issue on mobile when opening sidebar.
- Fix regression bug where auto-focusing on terms and related items no longer worked.
- Fix minor UI bug where dropdown menus for reordering terms were not appearing for new terms on small screens.
- Improve layout of long tags.

### Removed

## [5.4.0] - 2025-03-28

### Added

- Add "Stop editing" button.
- Add a "Copy to clipboard" button for each item.

### Changed

- Keep buttons and dropdown menus visible at the top.

## [5.3.1] - 2025-03-28

### Fixed

- Work around a [bug](https://github.com/drag-drop-touch-js/dragdroptouch/issues/90) in a 3rd party library that was making all links draggable on mobile devices.
- Allow scrolling horizontally within item cards instead of allowing text to overflow (and mess up the page layout).
- When save request to back end fails, scroll to bottom to ensure error message is visible.

## [5.3.0] - 2025-03-18

### Added

- Add a filter field for the index on larger screens.
- Add buttons and drag & drop handles for rearranging terms.
- Add drag & drop handles for rearranging related items.

### Changed

- Raise recommended maximum number of items from 500 to 1,000.
- On larger screens, move search box/button to the right side of the page.
- Rearrange layout of buttons for rearranging/deleting terms and related items.
- Refine edit conflict detection mechanism. This should only take effect for Node.js back ends that use the `worker.min.js` script.

### Fixed

- Hide "Back to top" link when opening search or single item dialog.
- Improve dark mode colours for search button.
- Improve layout of related items in form on edit page.
- Improve layout of buttons and select menus with long text.
- When pressing up/down buttons for rearranging related items, make the focus follow the button for the item being moved.
- Fix bug where not doing percent-decoding on fragment identifiers in URLs broke item highlighting.

## [5.2.1] - 2024-12-09

### Fixed

- Make `Browser.application` hack more reliable, as v5.2.0 had accidentally been released with a bug that broke the ability to open a glossary file in the browser.

## [5.2.0] - 2024-12-09

### Changed

- Improve performance.
- Improve styling.
  - Increase background/foreground contrast for a number of elements.
  - Change styling of index of terms.

### Fixed

- Keep links to related items intact when renaming those items.

## [5.1.0] - 2024-11-18

### Added

- Allow setting a default theme. This is used if the user has not explicitly picked a theme, and has also not indicated a preference for dark mode in their browser settings.

### Changed

- Allow viewing a glossary file directly in a browser (using a `file://` URL), instead of showing only a help message. This restores functionality that was lost from version 4.0.0 onwards (see #11 for context).

## [5.0.3] - 2024-07-17

### Changed

- Upgrade GitHub Actions.

### Fixed

- When using the `worker.min.js` script, ensure that items are sorted by the IDs of their disambiguated preferred terms.

## [5.0.2] - 2024-06-21

### Fixed

- Fix bug that always switched on saving changes in memory.
- Fix instructions visibility issue when `file:` URL is opened.

## [5.0.1] - 2024-06-21

This version contains the changes that were originally released in 4.7.0 (and then temporarily reverted in 4.7.1).

The **breaking** change is that there are now stricter validation rules around preferred and alternative terms. In particular, a preferred term cannot also appear as an alternative term in any item.

If your glossary does not violate this rule then upgrading should hopefully be easy. Otherwise, you can follow the steps below.

1. Upgrade to 5.0.1
2. Edit the glossary and save without making any changes
3. If this doesn't get past the validation, revert back to version 4.7.1 and then make the necessary changes
4. Go back to step 1.

### Changed

- Make link for scrolling back to top dynamically appear at bottom right when (probably) needed -- instead of always being visible at the top left.
- Open external links in a new tab.
- Show at most 40 search results.
- Upgrade KaTeX to version 0.16.10.
- Make the `worker.min.js` script more performant by removing UI specific concerns.

## [4.7.1] - 2024-06-21

This release brings back version 4.6.0. The changes for 4.7.0 should have been released as a new major version, as there are breaking changes.

## [4.7.0] - 2024-06-21

### Changed

- Make link for scrolling back to top dynamically appear at bottom right when (probably) needed -- instead of always being visible at the top left.
- Open external links in a new tab.
- Show at most 40 search results.
- Upgrade KaTeX to version 0.16.10.
- Make the `worker.min.js` script more performant by removing UI specific concerns.

## [4.6.0] - 2024-03-04

### Changed

- Make search also cover definitions.
- Support ⌘K for searching on macOS.
- Select first search result by default.
- Upgrade KaTeX to version 0.16.9.

### Fixed

- Fix issues with back/forward buttons.
- Stop using [:target](https://developer.mozilla.org/en-US/docs/Web/CSS/:target) for styling item pointed to by fragment identifier, as it [doesn't work well with back/forward buttons](https://github.com/whatwg/html/issues/639). Handle highlighting of item that has "focus" in JavaScript instead.
- Prevent having multiple items with the same fragment identifier. The fragment identifier for an item is its disambiguated preferred term, with spaces replaced by underscores.
- Prevent search item from being made active just because mouse pointer is already over it.

## [4.5.1] - 2024-02-13

### Fixed

- Fixed typo in test.

## [4.5.0] - 2024-02-13

### Added

- Add function for converting glossary JSON to HTML to experimental `worker.min.js` script.

### Changed

- Make it more obvious when a tag is being filtered by.
- Don't show tag being filtered by on item cards.
- Rename "Quick search" to just "Search" for consistency.

### Fixed

- Attempt to make jumping to fragment identifier more reliable on initial page load.

## [4.4.0] - 2024-01-24

### Added

- Include tags in Markdown export.
- Add experimental `worker.min.js` script to releases.

## [4.3.0] - 2024-01-08

### Added

- Allow exporting to JSON.

### Fixed

- Restore initial instructions for opening/editing a glossary. These were no longer being shown if the file was being opened directly in a browser (using a `file://` URL).

## [4.2.0] - 2023-12-16

### Added

- When viewing a single item, show its (disambiguated) preferred term in the page title.

### Changed

- Move all UI text to `Internationalisation` directory to facilitate translations (see issue #10).

### Fixed

- Fix minor bug where changes were applied in memory even if saving failed.
- Don't show select menu for disambiguation tag if there are no tags.
- Fix bug where button for making changes in memory was wrongly hidden.

## [4.1.0] - 2023-11-30

This release contains a (very minor) **breaking** change that only affects a specific scenario when editing a glossary. This scenario occurs when you reload the page if the URL contains query parameters (which are now used to store which tag is being filtered by and the item sort order). The Node.js script for the editor does not currently cope with query parameters and needs a small tweak -- please see below.

### How to upgrade from v4.0.0

1. Point the [static assets](https://github.com/hilverd/glossary-page-template#static-assets) to a version starting with `@4` or `@4.1`.
2. In the Node.js script at the bottom of the file, replace

```
var filePath = '.' + req.url;
```

with

```
var filePath = '.' + req.url.replace(/\?.*$/g, '');
```

### Added

- Support tags in Anki export.
- Use URL query parameter to save tag being filtered by.
- Show a warning in edit mode if there are more than 500 items, which is currently not recommended for performance reasons.

### Changed

- When creating a new item, if a tag is being filtered by then auto-tick that tag's checkbox.
- Use URL query parameter for `order-items-by` instead of `localStorage`.

### Fixed

- Fix minor HTML/UI bug -- don't give IDs to alternative terms, to prevent duplicates.
- Make clicking on a tag to filter by also scroll to the top.
- Improve formatting when exporting to Markdown.

## [4.0.0] - 2023-11-24

This release introduces support for _tags_, improves support for items that have terms in common, and removes support for "plain text" in favour of Markdown. It contains **breaking** changes that should only affect you if

* you have items with multiple definitions or are using plain text, or
* you are opening the glossary as a file in a browser (using a `file://` URL).

The latter now only shows the (initial) help message, and to see the actual glossary you will need to either use the built-in editor or "host" the `glossary.html` file using e.g. `python3 -m http.server 8000`.

Download [glossary.html](https://github.com/hilverd/glossary-page-template/releases/latest/download/glossary.html) to see some examples of the new features.

### How to upgrade from v3

1. Point the [static assets](https://github.com/hilverd/glossary-page-template#static-assets) to a version starting with `@4`.
2. If you were using plain text rather than Markdown, then inspect all terms and definitions and correct the formatting where needed.
3. Similarly, if you had items with multiple definitions, then you will want to inspect their definitions and correct the formatting where needed.

### Added

- Add support for tags.
  - You can use these to attach "labels" to items that people can then filter by. This may be useful for large glossaries that span multiple topics, where there is a need to categorize or group items. Tags can also be used to "disambiguate" items that have the same preferred term but whose meaning depends on some "context". For example, the term "default" has a different meaning in the context of computer science than it does in the context of finance.
- Add a small "Built using Glossary Page Template" footer to the list page.
- Add keyboard shortcut "e" for making changes.
- Show spinner while saving changes.

### Changed

- Only allow one definition per item, but allow items to have alternative terms in common.
- Make the concept of a _preferred term_ (and _alternative terms_) clearer in the UI.
- Show "Make changes" button when Node.js backend is running. This is to make it easier to see the glossary in "read-only" mode.

### Removed

- Remove support for plain text syntax in favour of Markdown.

## [3.8.0] - 2023-10-13

### Added

- Allow hiding "Order items" buttons in Settings.

### Changed

- Show terms beginning with 0-9 before those starting with a letter, and group them under "0–9" in the index.

## [3.7.0] - 2023-08-19

### Added

- Make it easier to rearrange related items.

### Changed

- Add button to easily copy command for running editor to clipboard.

## [3.6.1] - 2023-08-10

### Added

- Add logo to home page.

### Changed

- Use logo for favicons.

## [3.6.0] - 2023-08-09

### Added

- Allow ordering items "focused on" a particular term. Items that are closely related to this term (i.e. have a shorter path to the term's item via "See also" links) are shown earlier. For larger glossaries, this hopefully provides a way to focus on a certain topic by allowing the user to browse the items closely related to that topic, gradually moving towards topics that are further removed.

### Changed

- Upgrade KaTeX to version 0.16.8.

### Fixed

- Fix minor layout bug for "This field can't be empty" message.

## [3.5.2] - 2023-07-11

### Changed

- Use "definition" in the editor UI instead of "description" or "details" for consistency.

## [3.5.1] - 2023-06-30

### Changed

- Make minor improvement to styling of search results (use ellipsis unless math support is enabled).

### Fixed

- Fix layout bug that was causing a space to appear before each comma under "See also".
- Give search field focus more reliably.

## [3.5.0] - 2023-06-29

### Added

- In search results, show a preview of each term's definition.
- Style external links with an icon.

## [3.4.3] - 2023-06-27

### Changed

- Make icon for "View as single item" button more intuitive.

## [3.4.2] - 2023-06-25

### Fixed

- Fix bug where last updated date was not changing in "View as single item" modal dialog.

## [3.4.1] - 2023-06-25

### Added

- Add "View as single item" button on each card. This is available on "larger" screens and is meant to help make longer descriptions more readable.

### Changed

- Backfill last updated dates for example glossaries.

## [3.3.1] - 2023-06-13

### Added

- Save "order items" choice across browser sessions.

### Fixed

- Fix layout bug that removed the maximum height restriction for items.

## [3.3.0] - 2023-05-26

### Added

- Allow showing last updated dates for items.

## [3.2.0] - 2023-05-05

### Added

- Add button for choosing theme: light, dark, or operating system preference.

### Changed

- Upgrade KaTeX to version 0.16.7.

## [3.1.0] - 2023-04-30

### Added

- Make it clear that "n" is a keyboard shortcut for creating a new item.
- Glossary items can have a "Needs updating" badge.

### Changed

- When searching, terms for which the search string is a prefix are ranked higher than other terms.
- Rename "most frequent first" to "most mentioned first" for clarity.
- Improve print layout.

## [3.0.0] - 2023-03-20

### Added

- Support a Markdown-based syntax for the glossary title and for terms (not just term descriptions).
- When Markdown is enabled, support _math typesetting_ using [KaTeX](https://katex.org/).
  - See the Settings menu for how to enable this.
  - Inline math is written by surrounding an expression with dollar signs inside a code fragment: ` $e = mc^2$ `.
  - Display math is written using code blocks whose language is declared as `math`:

  ````
  ```math
  e = mc^2
  ```
  ````

### Fixed

- Make links in Markdown content not "tabbable" if a modal dialog is shown.

## [2.1.0] - 2023-03-11

### Added

- Close menu for mobile when escape key is pressed.
- Respect `prefers-reduced-motion` for mobile menu.
- Respect `prefers-contrast: more` on index page.

### Changed

- The term index now lists all terms that do not contain an alphabetic character (with or without diacritics) under `…`.
- Separate multiple definitions by a dotted line.

### Fixed

- Fix bug in logic for writing back to HTML file. Dollar signs need to be escaped.

## [2.0.0] - 2023-02-20

The main change in this release is that it introduces support for a _Markdown-based syntax_. Using that syntax, term definitions can now contain multiple paragraphs, bold/italic text, ordered/unordered lists, hyperlinks, images, code fragments, and more.

### How to upgrade from v1 and switch to Markdown

1. Point the [static assets](https://github.com/hilverd/glossary-page-template#static-assets) to a version starting with `@2`.
2. When making changes, open Settings at the top of the page and switch the input syntax to Markdown.
3. Inspect all term definitions and correct the formatting where needed.

### Added

- Support a [Markdown](https://commonmark.org/help/)-based syntax for term descriptions.
  - This uses [elm-markdown](https://github.com/dillonkearns/elm-markdown) which implements [GitHub Flavored Markdown](https://github.github.com/gfm/).

- Add a "Settings" section on the list page which allows changing the following.
  - How "compact" the overall layout is by adjusting the card widths.
  - Whether a Markdown-based syntax is enabled.
  - Whether the "Export" menu should be shown.

### Changed

- Switch to Source Sans 3 font. The Overpass font has [undesired behaviour for backticks](https://github.com/RedHatOfficial/Overpass/issues/95).
- Restrict item height and show a scrollbar for longer items, depending on overall layout determined by `data-card-width`.

### Fixed
### Removed

## [1.9.0] - 2023-02-01

### Added

- Show a preview on the "Edit Title and About Section" page.
- Show a preview on the edit/create item page.

### Fixed

- After deleting an item, focus should not be on another Delete button.

## [1.8.6] - 2023-01-28

### Added

- Add a README to `.tar.gz` package in a release with instructions on how to run the server for editing.

### Fixed

- Don't show "How to Make Changes" instructions if editor is already running.

## [1.8.5] - 2023-01-20

### Added

- Add `crossorigin` attribute to `link` element for CSS asset. This prepares it for having an `integrity` attribute added if people want to use [SRI](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity).

### Fixed

- Fix bug: do not ignore the `data-enable-help-for-making-changes` attribute.

## [1.8.4] - 2022-11-24

### Changed

- Order search results based on frequency.

## [1.7.4] - 2022-07-08

### Fixed

- Mention that the editor is meant to be used locally by a single user at a time.

## [1.7.3] - 2022-06-22

### Fixed

- Add new example to Vite config so it gets compiled.

## [1.7.2] - 2022-06-22

### Added

- Add Canada's Weather and meteorology glossary as an example.

## [1.7.1] - 2022-06-22

### Fixed

- Improve arrow key behaviour for export dropdown menu.

## [1.7.0] - 2022-06-22

### Added

- Add "Quick search" button.

## [1.6.3] - 2022-05-27

### Fixed

- When exporting to Anki, handle empty definitions with related items better.

## [1.6.2] - 2022-05-27

### Added

- Show "sandbox mode" message at top of main page if enabled.

## [1.6.1] - 2022-05-27

### Fixed

- Scroll to newly created items.

## [1.6.0] - 2022-05-27

### Added

- When `data-enable-saving-changes-in-memory="true"`, save in memory only. This allows running example glossaries in "sandbox mode" to see what the editor UI looks like.

## [1.5.0] - 2022-05-13

### Added

- Allow exporting as Anki deck (text file suitable for importing).

## [1.4.1] - 2022-05-13

### Added

- Add "Back to top" link at the top of the sidebar.

## [1.3.1] - 2022-05-05

### Added

- Improve styling and respect `prefers-reduced-motion`.

### Fixed

- Jump to anchor after page load.

## [1.3.0] - 2022-04-28

### Added

- Add favicons.
- Point `<meta name="generator" ...>` to `https://glossary.page/template`.

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

[Unreleased]: https://github.com/hilverd/glossary-page-template/compare/v5.4.0...HEAD
[5.4.0]: https://github.com/hilverd/glossary-page-template/compare/v5.3.1...v5.4.0
[5.3.1]: https://github.com/hilverd/glossary-page-template/compare/v5.3.0...v5.3.1
[5.3.0]: https://github.com/hilverd/glossary-page-template/compare/v5.2.1...v5.3.0
[5.2.1]: https://github.com/hilverd/glossary-page-template/compare/v5.2.0...v5.2.1
[5.2.0]: https://github.com/hilverd/glossary-page-template/compare/v5.1.0...v5.2.0
[5.1.0]: https://github.com/hilverd/glossary-page-template/compare/v5.0.3...v5.1.0
[5.0.3]: https://github.com/hilverd/glossary-page-template/compare/v5.0.2...v5.0.3
[5.0.2]: https://github.com/hilverd/glossary-page-template/compare/v5.0.1...v5.0.2
[5.0.1]: https://github.com/hilverd/glossary-page-template/compare/v4.7.1...v5.0.1
[4.7.1]: https://github.com/hilverd/glossary-page-template/compare/v4.7.0...v4.7.1
[4.7.0]: https://github.com/hilverd/glossary-page-template/compare/v4.6.0...v4.7.0
[4.6.0]: https://github.com/hilverd/glossary-page-template/compare/v4.5.1...v4.6.0
[4.5.1]: https://github.com/hilverd/glossary-page-template/compare/v4.5.0...v4.5.1
[4.5.0]: https://github.com/hilverd/glossary-page-template/compare/v4.4.0...v4.5.0
[4.4.0]: https://github.com/hilverd/glossary-page-template/compare/v4.3.0...v4.4.0
[4.3.0]: https://github.com/hilverd/glossary-page-template/compare/v4.2.0...v4.3.0
[4.2.0]: https://github.com/hilverd/glossary-page-template/compare/v4.1.0...v4.2.0
[4.1.0]: https://github.com/hilverd/glossary-page-template/compare/v4.0.0...v4.1.0
[4.0.0]: https://github.com/hilverd/glossary-page-template/compare/v3.8.0...v4.0.0
[3.8.0]: https://github.com/hilverd/glossary-page-template/compare/v3.7.0...v3.8.0
[3.7.0]: https://github.com/hilverd/glossary-page-template/compare/v3.6.1...v3.7.0
[3.6.1]: https://github.com/hilverd/glossary-page-template/compare/v3.6.0...v3.6.1
[3.6.0]: https://github.com/hilverd/glossary-page-template/compare/v3.5.2...v3.6.0
[3.5.2]: https://github.com/hilverd/glossary-page-template/compare/v3.5.1...v3.5.2
[3.5.1]: https://github.com/hilverd/glossary-page-template/compare/v3.5.0...v3.5.1
[3.5.0]: https://github.com/hilverd/glossary-page-template/compare/v3.4.3...v3.5.0
[3.4.3]: https://github.com/hilverd/glossary-page-template/compare/v3.4.2...v3.4.3
[3.4.2]: https://github.com/hilverd/glossary-page-template/compare/v3.4.1...v3.4.2
[3.4.1]: https://github.com/hilverd/glossary-page-template/compare/v3.3.1...v3.4.1
[3.3.1]: https://github.com/hilverd/glossary-page-template/compare/v3.3.0...v3.3.1
[3.3.0]: https://github.com/hilverd/glossary-page-template/compare/v3.2.0...v3.3.0
[3.2.0]: https://github.com/hilverd/glossary-page-template/compare/v3.1.0...v3.2.0
[3.1.0]: https://github.com/hilverd/glossary-page-template/compare/v3.0.0...v3.1.0
[3.0.0]: https://github.com/hilverd/glossary-page-template/compare/v2.1.0...v3.0.0
[2.1.0]: https://github.com/hilverd/glossary-page-template/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/hilverd/glossary-page-template/compare/v1.9.0...v2.0.0
[1.9.0]: https://github.com/hilverd/glossary-page-template/compare/v1.8.6...v1.9.0
[1.8.6]: https://github.com/hilverd/glossary-page-template/compare/v1.8.5...v1.8.6
[1.8.5]: https://github.com/hilverd/glossary-page-template/compare/v1.8.4...v1.8.5
[1.8.4]: https://github.com/hilverd/glossary-page-template/compare/v1.7.4...v1.8.4
[1.7.4]: https://github.com/hilverd/glossary-page-template/compare/v1.7.3...v1.7.4
[1.7.3]: https://github.com/hilverd/glossary-page-template/compare/v1.7.2...v1.7.3
[1.7.2]: https://github.com/hilverd/glossary-page-template/compare/v1.7.1...v1.7.2
[1.7.1]: https://github.com/hilverd/glossary-page-template/compare/v1.7.0...v1.7.1
[1.7.0]: https://github.com/hilverd/glossary-page-template/compare/v1.6.3...v1.7.0
[1.6.3]: https://github.com/hilverd/glossary-page-template/compare/v1.6.2...v1.6.3
[1.6.2]: https://github.com/hilverd/glossary-page-template/compare/v1.6.1...v1.6.2
[1.6.1]: https://github.com/hilverd/glossary-page-template/compare/v1.6.0...v1.6.1
[1.6.0]: https://github.com/hilverd/glossary-page-template/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/hilverd/glossary-page-template/compare/v1.4.1...v1.5.0
[1.4.1]: https://github.com/hilverd/glossary-page-template/compare/v1.3.1...v1.4.1
[1.3.1]: https://github.com/hilverd/glossary-page-template/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/hilverd/glossary-page-template/compare/v1.2.4...v1.3.0
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
