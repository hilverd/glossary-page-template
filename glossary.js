import './glossary.css';
import { Elm } from './src/ApplicationShell.elm';
import '@webcomponents/custom-elements';

const katexIsAvailable = typeof katex != "undefined";

const containerElement = document.getElementById('glossary-page-container');
const containerDataset = containerElement.dataset;

const enableHelpForMakingChanges = containerDataset.enableHelpForMakingChanges === 'true';
const enableSavingChangesInMemory = containerDataset.enableSavingChangesInMemory === 'true';
const enableMarkdownBasedSyntax = containerDataset.enableMarkdownBasedSyntax === 'true';
const enableExportMenu = containerDataset.enableExportMenu !== 'false';
const cardWidth = containerDataset.cardWidth;
const enableLastUpdatedDates = containerDataset.enableLastUpdatedDates === 'true';
const editorIsRunning = containerDataset.editorIsRunning === 'true';

const titleElement = document.getElementById('glossary-page-title');
const aboutElement = document.getElementById('glossary-page-about');

const glossaryElement = document.getElementById('glossary-page-items');

const aboutParagraph = aboutElement.querySelector('p').textContent.trim();

const aboutUlElement = aboutElement.querySelector('ul');
const aboutLiElements = Array.prototype.slice.apply(aboutUlElement.querySelectorAll('li'));
const aboutLinks = aboutLiElements.map(aboutLinkFromLiElement);

const dlElement = glossaryElement.querySelector('dl');
const glossaryItemDivElements = Array.prototype.slice.apply(dlElement.querySelectorAll('div'));
const glossaryItems = glossaryItemDivElements.map(glossaryItemFromDivElement);

function normaliseWhitespace(s) {
    return s.replace(/\s+/g, ' ').trim();
}

function aboutLinkFromLiElement(aboutLiElement) {
    const aElement = aboutLiElement.querySelector('a');
    return {
        href: aElement.getAttribute('href'),
        body: normaliseWhitespace(aElement.textContent)
    }
}

function glossaryItemFromDivElement(glossaryItemDivElement) {
    const dtElements = Array.prototype.slice.apply(glossaryItemDivElement.querySelectorAll('dt'));
    const ddElements = Array.prototype.slice.apply(glossaryItemDivElement.querySelectorAll('dd'));
    const relatedTermDdElements = ddElements.filter(ddElement => ddElement.getAttribute('class') === 'related-terms');
    const relatedTerms = (relatedTermDdElements.length > 0) ? glossaryItemRelatedTermFromDdElement(relatedTermDdElements[0]) : [];
    const needsUpdatingDdElements = ddElements.filter(ddElement => ddElement.getAttribute('class') === 'needs-updating');
    const lastUpdatedDate = glossaryItemDivElement.dataset.lastUpdated;

    return {
        terms: dtElements.map(glossaryItemTermFromDtElement),
        details: ddElements
            .filter(ddElement => ddElement.getAttribute('class') !== 'related-terms' && ddElement.getAttribute('class') !== 'needs-updating')
            .map(ddElement => ddElement.textContent.trim()),
        relatedTerms: relatedTerms,
        needsUpdating: needsUpdatingDdElements.length > 0,
        lastUpdatedDate: lastUpdatedDate
    }
}

function glossaryItemTermFromDtElement(dtElement) {
    const dfnElement = dtElement.querySelector('dfn');
    const id = dfnElement.id || null;
    const isAbbreviation = Boolean(dfnElement.querySelector('abbr'));
    const body = dfnElement.textContent;

    return {
        id: normaliseWhitespace(id),
        isAbbreviation: isAbbreviation,
        body: normaliseWhitespace(body)
    };
}

function glossaryItemRelatedTermFromDdElement(ddElement) {
    return Array.prototype.slice.apply(ddElement.querySelectorAll('a')).map(aElement => {
        const hrefAttribute = aElement.getAttribute('href');

        return {
            idReference: hrefAttribute.substring(1),
            body: normaliseWhitespace(aElement.textContent)
        };
    });
}

function reflectThemeInClassList() {
    if (localStorage.glossaryPageTheme === 'dark' || (!('glossaryPageTheme' in localStorage) &&
        window.matchMedia('(prefers-color-scheme: dark)').matches)) {
        document.documentElement.classList.add('dark');
    } else {
        document.documentElement.classList.remove('dark');
    }
}

const app = Elm.ApplicationShell.init({
    flags: {
        windowLocationHref: window.location.href,
        titleString: normaliseWhitespace(titleElement.textContent),
        aboutParagraph: aboutParagraph,
        aboutLinks: aboutLinks,
        glossaryItems: glossaryItems,
        editorIsRunning: editorIsRunning,
        enableHelpForMakingChanges: enableHelpForMakingChanges,
        enableSavingChangesInMemory: enableSavingChangesInMemory,
        enableExportMenu: enableExportMenu,
        enableMarkdownBasedSyntax: enableMarkdownBasedSyntax,
        enableLastUpdatedDates: enableLastUpdatedDates,
        theme: localStorage.glossaryPageTheme || "system",
        cardWidth: cardWidth,
        katexIsAvailable: katexIsAvailable
    }
});

app.ports.allowBackgroundScrolling.subscribe(() => {
    document.querySelector('body').classList.toggle('overflow-hidden', false);
});

app.ports.preventBackgroundScrolling.subscribe(() => {
    document.querySelector('body').classList.toggle('overflow-hidden', true);
});

app.ports.scrollSearchResultIntoView.subscribe((elementId) => {
    document.getElementById(elementId).scrollIntoView({ block: "nearest" });
});

app.ports.changeTheme.subscribe((themeName) => {
    if (themeName) {
        localStorage.glossaryPageTheme = themeName;
    } else {
        localStorage.removeItem('glossaryPageTheme');
    }

    reflectThemeInClassList();
});

app.ports.getCurrentDateTimeForSaving.subscribe(() => {
    app.ports.receiveCurrentDateTimeForSaving.send(new Date().toISOString());
});

function domReady(callback) {
    document.readyState === 'interactive' || document.readyState === 'complete' ?
        callback() : document.addEventListener('DOMContentLoaded', callback);
}

// Prevent FOUC
domReady(() => {
    reflectThemeInClassList();

    document.body.style.visibility = 'visible';

    // Try to keep the focus on the element that receives overall keyboard shortcuts like Control-K
    document.getElementById("glossary-page-outer")?.focus();

    document.addEventListener("focusout", (e) => {
        if (e.relatedTarget == null) {
            document.getElementById("glossary-page-outer")?.focus();
        }
    });

    const browserLocale =
        navigator.languages && navigator.languages.length
            ? navigator.languages[0]
            : navigator.language;

    const lastUpdatedDateOptions = { year: 'numeric', month: 'short', day: 'numeric' };

    customElements.define('last-updated',
        class extends HTMLElement {
            constructor() { super(); }
            connectedCallback() { this.setTextContent(); }
            attributeChangedCallback() { this.setTextContent(); }
            static get observedAttributes() { return []; }

            setTextContent() {
                const datetimeAttribute = this.getAttribute('datetime');

                if (datetimeAttribute)
                    this.textContent = new Date(datetimeAttribute).toLocaleDateString(browserLocale, lastUpdatedDateOptions);
                else
                    this.textContent = "unknown";
            }
        }
    );

    if (katexIsAvailable) {
        customElements.define('katex-inline',
            class extends HTMLElement {
                constructor() { super(); }
                connectedCallback() { this.setTextContent(); }
                attributeChangedCallback() { this.setTextContent(); }
                static get observedAttributes() { return ['data-expr']; }

                setTextContent() {
                    katex.render(this.dataset.expr, this, {
                        throwOnError: false
                    });

                }
            }
        );

        customElements.define('katex-display',
            class extends HTMLElement {
                constructor() { super(); }
                connectedCallback() { this.setTextContent(); }
                attributeChangedCallback() { this.setTextContent(); }
                static get observedAttributes() { return ['data-expr']; }

                setTextContent() {
                    katex.render(this.dataset.expr, this, {
                        displayMode: true,
                        throwOnError: false
                    });
                }
            }
        );
    }
});
