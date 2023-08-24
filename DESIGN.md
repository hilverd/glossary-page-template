# Design

These are notes about design decisions for this project. They describe the current state as well as ideas and plans for future changes. They will probably always be incomplete.

## Data model

The aim of the project is to provide an easy way to create and maintain a glossary. At the most basic level, a glossary is a list of _terms_ with (informal) _definitions_ of what those terms mean. A key assumption is that definitions are relatively short — a few hundred words at most. Longer definitions are also allowed but this is not the intended usage.

The contents of a glossary are stored in an HTML document, and the most appropriate way to model them seems to be the [`<dl>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl) (description list) element. This element contains zero or more groups each consisting of one or more `<dt>` (description term) elements followed by one or more `<dd>` (description details) elements. Each group is wrapped in a `<div>` as this is useful for styling and there appears to be good browser support for it. This project uses the term _(glossary) item_ to refer to a group.

If an item contains multiple terms then these are considered to be synonyms. The first term is called the _primary term_. If an item contains multiple definitions then these are interpreted to be different meanings that depend on some sort of context. For example, "Firefox" may refer to a web browser or to an animal. At the moment, there is no way to give names to contexts in the project's UI, and HTML also does not seem to have a standard way to do this.