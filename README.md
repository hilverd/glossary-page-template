<p align="center">
  <a href="https://glossary.page/template" target="_blank" rel="noopener noreferrer">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="img/logo-white.svg">
      <img width="180" alt="An open book with the letters A and Z visible and a magnifying glass hovering on top" src="img/logo.svg">
    </picture>
  </a>
</p>

# Glossary Page Template

This project makes it easy to create a glossary as a single HTML page that can be hosted anywhere.

## Features

* Includes integrated (single-user) editor UI that saves changes back to the HTML file — requires [Node.js](https://nodejs.org).
* Represents a glossary as a standard [`dl`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl) element for accessibility and portability.
* Supports different screen sizes using responsive design and includes dark mode support.
* Write in a Markdown-based syntax.
* Supports math typesetting using [KaTeX](https://katex.org/).

## Getting Started

Download [`glossary.html`](https://github.com/hilverd/glossary-page-template/releases/latest/download/glossary.html) from the [latest release](https://github.com/hilverd/glossary-page-template/releases/latest) and open it in a browser.

### Static assets

The HTML file points to `glstatic.net` for static assets hosted using Cloudflare:

```
https://glstatic.net/glossary-page-template@5/glossary.min.js
https://glstatic.net/glossary-page-template@5/glossary.min.css
```

The above assets are cached for 24 hours and get updated with any releases with a [major version number](https://semver.org/) of `5`. Alternatively you can change the URL to point to e.g. `@5.0` which gets all patch releases with version numbers of the form `5.0.x`. You can also use a specific version such as `5.0.3` — these are cached forever.

#### Security considerations

The approach above is the easiest one, but the [safest option](https://css-tricks.com/potential-dangers-of-third-party-javascript/) would be to either host the static assets yourself and/or use [Subresource Integrity](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity) (SRI) by specifying hashes for the static assets.

To host the static assets yourself, you can get them from the `.tar.gz` package in a release, which also contains a glossary template file that uses them.

To use SRI, change the two lines at the end of the `<head>` element so they

* point to a specific version of the JS and CSS files,
* have a `crossorigin` attribute, and
* have an `integrity="hash-here"` attribute.

You can get the `hash-here` values from the [release notes](https://github.com/hilverd/glossary-page-template/releases/latest).

## Known Limitations

* The integrated editor UI is meant to be run and used by a single local user only. It is not designed to be exposed to untrusted users.
* In its current state this project is not very suitable for glossaries of more than (say) 500 items. At the moment everything is stored in a single HTML file, and the DOM size and browser memory usage grows as the number of items does. (I'm planning to address this.)

## License

Distributed under the MIT License. See `LICENSE` for more information.

## Acknowledgments

This project is mainly written in [Elm](https://elm-lang.org/) and relies on [elm-markdown](https://github.com/dillonkearns/elm-markdown) for parsing and rendering Markdown. The UI is based on components from [Tailwind UI](https://tailwindui.com/). Most icons are from either [Heroicons](https://heroicons.com/) or [Lucide](https://lucide.dev/).

Thanks to everyone who contributed suggestions for improvements.
