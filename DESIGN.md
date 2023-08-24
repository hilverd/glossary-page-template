# Design

These are notes about design decisions for this project. They describe the current state as well as ideas and plans for future changes. They will probably always be incomplete.

## Data model

The aim of the project is to provide an easy way to create and maintain a glossary. At a basic level, a glossary is a list of _terms_ with (informal) _definitions_ of what those terms mean. A key assumption is that definitions are relatively short — a few hundred words at most. Longer definitions are also allowed but this is not the intended usage.

The contents of a glossary are stored in an HTML document, and the most appropriate way to model them seems to be the [`<dl>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl) (description list) element. This element contains zero or more groups each consisting of one or more `<dt>` (description term) elements followed by one or more `<dd>` (description details) elements. Each group is wrapped in a `<div>` as this is useful for styling and there appears to be good browser support for it. This project uses the term _(glossary) item_ to refer to a group.

If an item contains multiple terms then these are considered to be synonyms. The first term is called the _primary term_. If an item contains multiple definitions then these are interpreted to be different meanings that depend on some sort of context. For example, "Firefox" may refer to a web browser or to an animal. At the moment, there is no way to give names to contexts in the project's UI, and HTML also does not seem to have a standard way to do this.

Items can also point to zero or more _related terms_. In the HTML, these are listed in a final `<dd>` element for the item as there doesn't seem to be anything more idiomatic.

## Moving towards support for tags

For larger glossaries, it seems tempting to add support for "tags" that can be applied to items (or terms/definitions inside items), and then shown in the UI as well as used for filtering. For example, one might want to mark certain terms as being "technical" in nature and allow users to easily hide them from view. Tags could also be used to explicitly label "contexts" or "(sub)topics" for items that have multiple definitions. In the context of "information technology", the meaning of the term "Apple" is different than in the context of "culinary fruits".

It feels like at this point it would be good to carefully consider how the data model and UI for the project might be improved and extended to support tags.

A glossary is a basic type of _controlled vocabulary_. More sophisticated controlled vocabularies such as thesauri, taxonomies and ontologies provide more details about properties of terms and how they are linked (e.g. by specifying antonyms or pointing to "broader" and "narrower" terms). Most people might not be very familiar with the differences between those things, so it seems sensible to keep using the word "glossary" even if it might eventually offer more advanced features than would typically be expected.

We can take inspiration from how [SKOS](https://www.w3.org/TR/skos-primer/) (and later possibly [OWL](https://www.w3.org/TR/owl2-primer/)) represent knowledge. Instead of "primary term" we should say "preferred term". Other terms are "alternative terms".