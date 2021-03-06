# Glossary Page Template

This project makes it easy to create a glossary as a single HTML page that can be hosted anywhere.

## Features

* Includes integrated (single-user) editor UI that saves changes back to the HTML file.
* Represents a glossary as a standard [`dl`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl) element for accessibility and portability.
* Supports different screen sizes using responsive design and includes dark mode support.

## Getting Started

Download `glossary.html` from the [latest release](https://github.com/hilverd/glossary-page-template/releases/latest) and open it in a browser.

### Static assets

The `glossary.html` file points to `glstatic.net` for static assets hosted using Cloudflare:

```
https://glstatic.net/glossary-page-template@1/glossary.min.js
https://glstatic.net/glossary-page-template@1/glossary.min.css
```

These files are cached for 24 hours and get updated with any releases with a [major version number](https://semver.org/) of `1`. Alternatively you can change the URL to point to e.g. `@1.1` which gets all patch releases with version numbers of the form `1.1.x`. You can also specify a specific version such as `1.0.1` -- these are cached forever.

If you [prefer](https://css-tricks.com/potential-dangers-of-third-party-javascript/) to host the static assets yourself then you can get them from the `.tar.gz` package in a release, which also contains a glossary template file that uses them.

## Development

You'll need to have [pnpm](https://pnpm.io/) installed.

```
git clone https://github.com/hilverd/glossary-page-template.git
cd glossary-page-template
pnpm install
pnpm run dev
```

Then open http://localhost:3000/glossary.html in a browser.

### Building a release from sources

```
pnpm run build
./bin/prepare-release
```
