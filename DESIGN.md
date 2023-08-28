# Design

These are notes about design decisions for this project. They describe the current state as well as ideas and plans for future changes. They will probably always be incomplete.

## Data model

The aim of the project is to provide an easy way to create and maintain a glossary. At a basic level, a glossary is a list of _terms_ with (informal) _definitions_ of what those terms mean. Two key assumptions made here are that glossaries are focused on a specific subject area (so not as broad as a dictionary) and that definitions are relatively short — a few hundred words at most. Longer definitions are also allowed but this is not the intended usage.

The contents of a glossary are stored in an HTML document, and the most appropriate way to model them seems to be the [`<dl>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl) (description list) element. This element contains zero or more groups each consisting of one or more `<dt>` (description term) elements followed by one or more `<dd>` (description details) elements. Each group is wrapped in a `<div>` as this is useful for styling and there appears to be good browser support for it. This project uses the term _(glossary) item_ to refer to a group.

If an item contains multiple terms then these are considered to be synonyms. The first term is called the _primary term_. If an item contains multiple definitions then these are interpreted to be different meanings that depend on some sort of context. For example, "Firefox" may refer to a web browser or to an animal. At the moment, there is no way to give names to contexts in the project's UI, and HTML also does not seem to have a standard way to do this.

Items can also point to zero or more _related terms_. In the HTML, these are listed in a final `<dd>` element for the item as there doesn't seem to be anything more idiomatic.

## Moving towards support for tags

For larger glossaries, it seems tempting to add support for "tags" that can be applied to items (or terms/definitions inside items), and then shown in the UI for browsing and filtering. For instance, one might want to mark certain terms as being "technical" in nature and allow users to easily hide them from view. Tags could also be used to explicitly label "contexts" or "(sub)topics" for items that have multiple definitions. As an example, in the context of "information technology" the meaning of the term "Apple" is different than in the context of "culinary fruits".

It feels like at this point it would be good to carefully consider how the data model and UI for the project might be improved and extended to support tags.

A glossary is a basic type of _controlled vocabulary_. Compared to a typical glossary, more sophisticated controlled vocabularies such as thesauri, taxonomies and ontologies provide additional details about properties of terms and how they are linked (e.g. by specifying antonyms or pointing to "broader" and "narrower" terms). Most people might not be very familiar with the differences between those things, so it seems reasonable to keep using the word "glossary" for this project even if it might eventually offer more advanced features than in a typical glossary.

We can take inspiration from how [SKOS](https://www.w3.org/TR/skos-primer/) (and later possibly [OWL](https://www.w3.org/TR/owl2-primer/)) represent knowledge. As a first step, "primary term" could be renamed to "preferred term" (corresponding to a "preferred label" in SKOS), and other terms could be called "alternative terms" (corresponding to "alternative labels" in SKOS).

### Representing multiple definitions for a term

Returning to the "Firefox" example, in SKOS this might be modelled as follows.

```
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .

<http://example.org/concepts/FirefoxAnimal> a skos:Concept ;
    skos:prefLabel "Firefox (Animal)"@en ;
    skos:altLabel "Firefox"@en ;

<http://example.org/concepts/FirefoxBrowser> a skos:Concept ;
    skos:prefLabel "Firefox (Web Browser)"@en ;
    skos:altLabel "Firefox"@en ;
    skos:related <http://example.org/concepts/Safari> .

<http://example.org/concepts/Safari> a skos:Concept ;
    skos:prefLabel "Safari"@en ;
    skos:related <http://example.org/concepts/FirefoxBrowser> .
```

This is a bit different from the current model of having a single "item" with multiple definitions. Instead, each definition would be represented by a separate SKOS concept (and concepts may be linked using `skos:related`). In general, distinct concepts cannot have the same preferred label, but they can share alternative labels. So another step would be to no longer allow multiple definitions per item in the data model and UI, but instead require each to have its own concept with a unique preferred label. It remains to be seen how the index and search features would need to change to support this.