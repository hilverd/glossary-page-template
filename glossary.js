import './glossary.css';
import { Elm } from './src/ApplicationShell.elm';
import '@webcomponents/custom-elements';

const katexIsAvailable = typeof katex != "undefined";

const containerElement = document.getElementById('glossary-page-container');

if (containerElement) {
    const containerDataset = containerElement.dataset;

    const enableHelpForMakingChanges = containerDataset.enableHelpForMakingChanges === 'true';
    const enableSavingChangesInMemory = containerDataset.enableSavingChangesInMemory === 'true';
    const enableExportMenu = containerDataset.enableExportMenu !== 'false';
    const enableOrderItemsButtons = containerDataset.enableOrderItemsButtons !== 'false';
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

    const tagDivElements = Array.prototype.slice.apply(document.querySelectorAll('#glossary-page-tags > dl > div'));

    const tagsWithDescriptions = tagDivElements.map(tagDivElement => {
        return {
            tag: tagDivElement.querySelector('dt').textContent.trim(),
            description: tagDivElement.querySelector('dd').textContent.trim()
        }
    });

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

    function tagInItemFromButtonElement(buttonElement) {
        return buttonElement?.textContent?.trim();
    }

    function glossaryItemFromDivElement(glossaryItemDivElement) {
        const dtElements = Array.prototype.slice.apply(glossaryItemDivElement.querySelectorAll('dt'));
        const preferredTermDtElement = dtElements[0];
        const alternativeTermsDtElements = dtElements.slice(1);
        const ddElements = Array.prototype.slice.apply(glossaryItemDivElement.querySelectorAll('dd'));

        const definitionDdElements = ddElements
            .filter(ddElement =>
                ddElement.className !== 'tags' &&
                ddElement.className !== 'needs-updating' &&
                ddElement.className !== 'related-terms'
            );

        /* The current implementation allows one definition per item.
           Previous versions allowed multiple definitions, and for backwards compatibility these are being joined here.
        */
        const definition = definitionDdElements.map(ddElement => ddElement.textContent.trim()).join('\n\n');

        const tagsDdElement = ddElements.filter(ddElement => ddElement.className === 'tags')[0];
        const tagElements = Array.prototype.slice.apply(tagsDdElement?.querySelectorAll('button') || []);
        const tags = tagElements.map(tagElement => tagInItemFromButtonElement(tagElement));
        const hasDisambiguationTag = preferredTermDtElement.querySelector('dfn span.disambiguation') !== null;

        const needsUpdatingDdElements = ddElements.filter(ddElement => ddElement.className === 'needs-updating');
        const relatedTermDdElements = ddElements.filter(ddElement => ddElement.className === 'related-terms');
        const relatedTerms = (relatedTermDdElements.length > 0) ? glossaryItemRelatedTermFromDdElement(relatedTermDdElements[0]) : [];
        const lastUpdatedDate = glossaryItemDivElement.dataset.lastUpdated;

        return {
            preferredTerm: glossaryItemTermFromDtElement(preferredTermDtElement),
            alternativeTerms: alternativeTermsDtElements.map(glossaryItemTermFromDtElement),
            disambiguationTag: hasDisambiguationTag ? tags[0] : null,
            normalTags: hasDisambiguationTag ? tags.slice(1) : tags,
            definition: definition || null,
            relatedTerms: relatedTerms,
            needsUpdating: needsUpdatingDdElements.length > 0,
            lastUpdatedDate: lastUpdatedDate
        }
    }

    function glossaryItemTermFromDtElement(dtElement) {
        const dfnElement = dtElement.querySelector('dfn');
        const isAbbreviation = Boolean(dfnElement.querySelector('abbr'));
        const dfnElementWithoutDisambiguationTag = dfnElement.querySelector('span') || dfnElement;
        const body = dfnElementWithoutDisambiguationTag.textContent;

        return {
            isAbbreviation: isAbbreviation,
            body: normaliseWhitespace(body)
        };
    }

    function glossaryItemRelatedTermFromDdElement(ddElement) {
        return Array.prototype.slice.apply(ddElement.querySelectorAll('a')).map(aElement => {
            const hrefAttribute = aElement.getAttribute('href');

            return {
                id: hrefAttribute.substring(1),
                isAbbreviation: false, // ignored
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
            titleString: normaliseWhitespace(titleElement.textContent),
            aboutParagraph: aboutParagraph,
            aboutLinks: aboutLinks,
            tagsWithDescriptions: tagsWithDescriptions,
            glossaryItems: glossaryItems,
            editorIsRunning: editorIsRunning,
            enableHelpForMakingChanges: enableHelpForMakingChanges,
            enableSavingChangesInMemory: enableSavingChangesInMemory,
            enableExportMenu: enableExportMenu,
            enableOrderItemsButtons: enableOrderItemsButtons,
            enableLastUpdatedDates: enableLastUpdatedDates,
            theme: localStorage.glossaryPageTheme || 'system',
            orderItemsBy: (enableOrderItemsButtons && localStorage.orderItemsBy) || 'alphabetically',
            cardWidth: cardWidth,
            katexIsAvailable: katexIsAvailable
        }
    });

    function allowBackgroundScrolling() {
        document.querySelector('body').classList.toggle('overflow-hidden', false);
    }

    function waitForElement(elementId) {
        return new Promise(resolve => {
            if (document.getElementById(elementId)) {
                return resolve(document.getElementById(elementId));
            }

            const observer = new MutationObserver(mutations => {
                if (document.getElementById(elementId)) {
                    resolve(document.getElementById(elementId));
                    observer.disconnect();
                }
            });

            observer.observe(document.body, {
                childList: true,
                subtree: true
            });
        });
    }

    const untilAsync = async (fn, time = 1000, wait = 10000) => {
        const startTime = new Date().getTime();
        for (; ;) {
            try {
                if (await fn()) {
                    return true;
                }
            } catch (e) {
                throw e;
            }

            if (new Date().getTime() - startTime > wait) {
                throw new Error('Timed out waiting for condition to become true.');
            } else {
                await new Promise((resolve) => setTimeout(resolve, time));
            }
        }
    };

    app.ports.allowBackgroundScrolling.subscribe(() => {
        allowBackgroundScrolling();
    });

    app.ports.preventBackgroundScrolling.subscribe(() => {
        document.querySelector('body').classList.toggle('overflow-hidden', true);
    });

    app.ports.scrollElementIntoView.subscribe((elementId) => {
        const elem = document.getElementById(elementId);

        if (elem) {
            elem.scrollIntoView({ block: "nearest" });
        } else {
            // Do this just in case, as it seems that there might be situations where the background is left "locked".
            allowBackgroundScrolling();
        }
    });

    app.ports.giveSearchFieldFocusOnceItIsPresent.subscribe((elementId) => {
        waitForElement(elementId).then(async (element) => {
            try {
                await untilAsync(() => {
                    if (element)
                        element.focus();

                    document.activeElement === document.getElementById(elementId);
                }, 50, 2000)
            } catch (e) {
                // ignore
            }
        });
    });

    app.ports.scrollSearchResultIntoView.subscribe((elementId) => {
        const elem = document.getElementById(elementId);

        if (elem) {
            elem.scrollIntoView({ block: "nearest" });
        }
    });

    app.ports.changeTheme.subscribe((themeName) => {
        if (themeName) {
            localStorage.glossaryPageTheme = themeName;
        } else {
            localStorage.removeItem('glossaryPageTheme');
        }

        reflectThemeInClassList();
    });

    app.ports.changeOrderItemsBy.subscribe((orderItemsBy) => {
        localStorage.orderItemsBy = orderItemsBy;
    });

    app.ports.getCurrentDateTimeForSaving.subscribe(() => {
        app.ports.receiveCurrentDateTimeForSaving.send(new Date().toISOString());
    });

    app.ports.copyEditorCommandToClipboard.subscribe((textToCopy) => {
        navigator.clipboard.writeText(textToCopy).then(
            () => {
                app.ports.attemptedToCopyEditorCommandToClipboard.send(true);
            },
            () => {
                app.ports.attemptedToCopyEditorCommandToClipboard.send(false);
            },
        );
    });

    app.ports.selectAllInTextFieldWithCommandToRunEditor.subscribe(() => {
        document.getElementById("glossary-page-text-field-with-command-to-run-editor")?.select();
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
                static get observedAttributes() { return ['datetime']; }

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
}