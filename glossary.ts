import '@webcomponents/custom-elements';
import './glossary.css';
import { Elm } from './src/ApplicationShell.elm';
import { untilAsync, waitForElement } from './ts/utilities.ts';

const katexIsAvailable: boolean = typeof katex != "undefined";

const containerElement: HTMLElement | null = document.getElementById('glossary-page-container');

if (containerElement) {
    const containerDataset: DOMStringMap = containerElement.dataset;

    const enableHelpForMakingChanges: boolean = containerDataset.enableHelpForMakingChanges === 'true';
    const enableSavingChangesInMemory: boolean = containerDataset.enableSavingChangesInMemory === 'true';
    const enableExportMenu: boolean = containerDataset.enableExportMenu !== 'false';
    const enableOrderItemsButtons: boolean = containerDataset.enableOrderItemsButtons !== 'false';
    const cardWidth: string | undefined = containerDataset.cardWidth;
    const enableLastUpdatedDates: boolean = containerDataset.enableLastUpdatedDates === 'true';
    const editorIsRunning: boolean = containerDataset.editorIsRunning === 'true';

    const titleElement: HTMLElement | null = document.getElementById('glossary-page-title');
    const aboutElement: HTMLElement | null = document.getElementById('glossary-page-about');

    const glossaryElement: HTMLElement | null = document.getElementById('glossary-page-items');

    const aboutParagraph: string = aboutElement?.querySelector('p')?.textContent?.trim() || '';

    const aboutUlElement: HTMLElement | null = aboutElement?.querySelector('ul') || null;
    const aboutLiElements: HTMLElement[] = Array.prototype.slice.apply(aboutUlElement?.querySelectorAll('li') || []);
    const aboutLinks: { href: string, body: string }[] = aboutLiElements.map(aboutLinkFromLiElement);

    const tagDivElements: HTMLElement[] = Array.prototype.slice.apply(document.querySelectorAll('#glossary-page-tags > dl > div'));

    const tagsWithDescriptions: { tag: string, description: string }[] = tagDivElements.map(tagDivElement => {
        return {
            tag: tagDivElement.querySelector('dt')?.textContent?.trim() || '',
            description: tagDivElement.querySelector('dd')?.textContent?.trim() || ''
        }
    });

    const dlElement: HTMLElement | null = glossaryElement?.querySelector('dl') || null;
    const glossaryItemDivElements: HTMLElement[] = Array.prototype.slice.apply(dlElement?.querySelectorAll('div') || []);
    const glossaryItems: any[] = glossaryItemDivElements.map(glossaryItemFromDivElement);

    function normaliseWhitespace(s: string): string {
        return s.replace(/\s+/g, ' ').trim();
    }

    function aboutLinkFromLiElement(aboutLiElement: HTMLElement): { href: string, body: string } {
        const aElement: HTMLAnchorElement | null = aboutLiElement.querySelector('a');
        return {
            href: aElement?.getAttribute('href') || '',
            body: normaliseWhitespace(aElement?.textContent || '')
        }
    }

    function tagInItemFromButtonElement(buttonElement: HTMLButtonElement): string | undefined {
        return buttonElement?.textContent?.trim();
    }

    function glossaryItemFromDivElement(glossaryItemDivElement: HTMLElement): any {
        const dtElements: HTMLElement[] = Array.prototype.slice.apply(glossaryItemDivElement.querySelectorAll('dt'));
        const preferredTermDtElement: HTMLElement | undefined = dtElements[0];
        const alternativeTermsDtElements: HTMLElement[] = dtElements.slice(1);
        const ddElements: HTMLElement[] = Array.prototype.slice.apply(glossaryItemDivElement.querySelectorAll('dd'));

        const definitionDdElements: HTMLElement[] = ddElements
            .filter(ddElement =>
                ddElement.className !== 'tags' &&
                ddElement.className !== 'needs-updating' &&
                ddElement.className !== 'related-terms'
            );

        /* The current implementation allows one definition per item.
           Previous versions allowed multiple definitions, and for backwards compatibility these are being joined here.
        */
        const definition: string = definitionDdElements.map(ddElement => ddElement.textContent?.trim()).join('\n\n') || '';

        const tagsDdElement: HTMLElement | undefined = ddElements.filter(ddElement => ddElement.className === 'tags')[0];
        const tagElements: HTMLButtonElement[] = Array.prototype.slice.apply(tagsDdElement?.querySelectorAll('button') || []);
        const tags: (string | undefined)[] = tagElements.map(tagElement => tagInItemFromButtonElement(tagElement));
        const hasDisambiguationTag: boolean = preferredTermDtElement?.querySelector('dfn span.disambiguation') !== null;

        const needsUpdatingDdElements: HTMLElement[] = ddElements.filter(ddElement => ddElement.className === 'needs-updating');
        const relatedTermDdElements: HTMLElement[] = ddElements.filter(ddElement => ddElement.className === 'related-terms');
        const relatedTerms: any[] = (relatedTermDdElements.length > 0) ? glossaryItemRelatedTermFromDdElement(relatedTermDdElements[0]) : [];
        const lastUpdatedDate: string | undefined = glossaryItemDivElement.dataset.lastUpdated;
        const lastUpdatedByName: string | undefined = glossaryItemDivElement.dataset.lastUpdatedByName;
        const lastUpdatedByEmailAddress: string | undefined = glossaryItemDivElement.dataset.lastUpdatedByEmailAddress;

        return {
            preferredTerm: glossaryItemTermFromDtElement(preferredTermDtElement),
            alternativeTerms: alternativeTermsDtElements.map(glossaryItemTermFromDtElement),
            disambiguationTag: hasDisambiguationTag ? tags[0] : null,
            normalTags: hasDisambiguationTag ? tags.slice(1) : tags,
            definition: definition || null,
            relatedTerms: relatedTerms,
            needsUpdating: needsUpdatingDdElements.length > 0,
            lastUpdatedDate: lastUpdatedDate || null,
            lastUpdatedByName: lastUpdatedByName || null,
            lastUpdatedByEmailAddress: lastUpdatedByEmailAddress || null
        }
    }

    function glossaryItemTermFromDtElement(dtElement: HTMLElement | undefined): { isAbbreviation: boolean, body: string } {
        const dfnElement: HTMLElement | null = dtElement?.querySelector('dfn') || null;
        const isAbbreviation: boolean = Boolean(dfnElement?.querySelector('abbr'));
        const dfnElementWithoutDisambiguationTag: HTMLElement | null = dfnElement?.querySelector('span') || dfnElement;
        const body: string = dfnElementWithoutDisambiguationTag?.textContent || '';

        return {
            isAbbreviation: isAbbreviation,
            body: normaliseWhitespace(body)
        };
    }

    function glossaryItemRelatedTermFromDdElement(ddElement: HTMLElement): any[] {
        return Array.prototype.slice.apply(ddElement.querySelectorAll('a')).map(aElement => {
            const hrefAttribute: string | null = aElement.getAttribute('href');

            return {
                id: hrefAttribute?.substring(1),
                isAbbreviation: false, // ignored
                body: normaliseWhitespace(aElement.textContent || '')
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
            titleString: normaliseWhitespace(titleElement?.textContent || ''),
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
            cardWidth: cardWidth,
            katexIsAvailable: katexIsAvailable
        }
    });

    function allowBackgroundScrolling() {
        document.querySelector('body')?.classList.toggle('overflow-hidden', false);
    }

    app.ports.allowBackgroundScrolling.subscribe(() => {
        allowBackgroundScrolling();
    });

    app.ports.preventBackgroundScrolling.subscribe(() => {
        document.querySelector('body')?.classList.toggle('overflow-hidden', true);
    });

    app.ports.scrollElementIntoView.subscribe((elementId: string) => {
        const elem: HTMLElement | null = document.getElementById(elementId);

        if (elem) {
            elem.scrollIntoView({ block: "nearest" });
        } else {
            // Do this just in case, as it seems that there might be situations where the background is left "locked".
            allowBackgroundScrolling();
        }
    });

    app.ports.giveSearchFieldFocusOnceItIsPresent.subscribe((elementId: string) => {
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

    app.ports.scrollSearchResultIntoView.subscribe((elementId: string) => {
        const elem: HTMLElement | null = document.getElementById(elementId);

        if (elem) {
            elem.scrollIntoView({ block: "nearest" });
        }
    });

    app.ports.changeTheme.subscribe((themeName: string) => {
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

    app.ports.copyEditorCommandToClipboard.subscribe((textToCopy: string) => {
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
        const element: HTMLElement | null = document.getElementById("glossary-page-text-field-with-command-to-run-editor");

        if (element instanceof HTMLInputElement) {
            const textField = element;

            textField.select();
        }
    });

    function domReady(callback: () => void) {
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

        const browserLocale: string =
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
                    const datetimeAttribute: string | null = this.getAttribute('datetime');

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