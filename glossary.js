import './glossary.css';
import { Elm } from './src/ApplicationShell.elm';

const containerElement = document.getElementById('glossary-page-container');
const titleElement = document.getElementById('glossary-page-title');
const aboutElement = document.getElementById('glossary-page-about');

const glossaryElement = document.getElementById('glossary-page-items');

const aboutParagraph = normaliseWhitespace(aboutElement.querySelector('p').textContent);

const aboutUlElement = aboutElement.querySelector('ul');
const aboutLiElements = Array.prototype.slice.apply(aboutUlElement.querySelectorAll('li'));
const aboutLinks = aboutLiElements.map(aboutLinkFromLiElement);

const runningInDevelopment = import.meta.env.DEV;
const editorIsRunning = containerElement.getAttribute('data-editor-is-running') === 'true' || runningInDevelopment;

const enableHelpForMakingChanges = containerElement.getAttribute('data-enable-help-for-making-changes') === 'true';
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

    return {
        terms: dtElements.map(glossaryItemTermFromDtElement),
        details: ddElements
            .filter(ddElement => ddElement.getAttribute('class') !== 'related-terms')
            .map(ddElement => normaliseWhitespace(ddElement.textContent)),
        relatedTerms: relatedTerms
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

const app = Elm.ApplicationShell.init({
    flags: {
        windowLocationHref: window.location.href,
        titleString: normaliseWhitespace(titleElement.textContent),
        aboutParagraph: aboutParagraph,
        aboutLinks: aboutLinks,
        glossaryItems: glossaryItems,
        editorIsRunning: editorIsRunning,
        enableHelpForMakingChanges: enableHelpForMakingChanges
    }
});

app.ports.allowBackgroundScrolling.subscribe(() => {
    document.querySelector('body').classList.toggle('overflow-hidden', false);
});

app.ports.preventBackgroundScrolling.subscribe(() => {
    document.querySelector('body').classList.toggle('overflow-hidden', true);
});

function domReady(callback) {
    document.readyState === 'interactive' || document.readyState === 'complete' ?
        callback() : document.addEventListener('DOMContentLoaded', callback);
}

// Prevent FOUC
domReady(() => {
    document.body.style.visibility = 'visible';
});
