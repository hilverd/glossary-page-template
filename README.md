# Glossary Page Template

This project makes it easy to create a glossary as a single HTML page that can be hosted anywhere.

## Features

* Includes integrated (single-user) editor UI that saves changes back to the HTML file.
* Represents a glossary as a standard [`dl`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl) element for accessibility and portability.
* Supports different screen sizes using responsive design and includes dark mode support.
* Write in either plain text or a Markdown-based syntax.
* Supports math typesetting using [KaTeX](https://katex.org/).

## Getting Started

Download `glossary.html` from the [latest release](https://github.com/hilverd/glossary-page-template/releases/latest) and open it in a browser.

### Static assets

The `glossary.html` file points to `glstatic.net` for static assets hosted using Cloudflare:

```
https://glstatic.net/glossary-page-template@3/glossary.min.js
https://glstatic.net/glossary-page-template@3/glossary.min.css
```

These files are cached for 24 hours and get updated with any releases with a [major version number](https://semver.org/) of `3`. Alternatively you can change the URL to point to e.g. `@3.0` which gets all patch releases with version numbers of the form `3.0.x`. You can also use a specific version such as `3.0.0` -- these are cached forever.

#### Security considerations

The [safest option](https://css-tricks.com/potential-dangers-of-third-party-javascript/) would be to either host the static assets yourself or use [Subresource Integrity](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity) (SRI) by specifying hashes for the static assets.

To host the static assets yourself, you can get them from the `.tar.gz` package in a release, which also contains a glossary template file that uses them.

To use SRI, change the two lines at the end of the `<head>` element so they

* point to a specific version of the JS and CSS files,
* have a `crossorigin` attribute, and
* have an `integrity="hash-here"` attribute.

You can get the `hash-here` values from the [release notes](https://github.com/hilverd/glossary-page-template/releases/latest).

## Limitations

* The integrated editor UI is meant to be run and used by a single local user only. It is not designed to be exposed to untrusted users.
