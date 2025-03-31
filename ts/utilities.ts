export const untilAsync = async (fn: () => Promise<boolean>, time: number = 1000, wait: number = 10000): Promise<boolean> => {
    const startTime: number = new Date().getTime();
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

export function waitForElement(elementId: string): Promise<HTMLElement> {
    return new Promise(resolve => {
        if (document.getElementById(elementId)) {
            return resolve(document.getElementById(elementId) as HTMLElement);
        }

        const observer = new MutationObserver(mutations => {
            if (document.getElementById(elementId)) {
                resolve(document.getElementById(elementId) as HTMLElement);
                observer.disconnect();
            }
        });

        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    });
}

export function normaliseWhitespace(s: string): string {
    return s.replace(/\s+/g, ' ').trim();
}

export function scrollFragmentIdentifierIntoView() {
    const hash = window.location.hash;

    if (hash !== '') {
        const elementId = decodeURIComponent(hash.slice(1));
        const elem = document.getElementById(elementId);

        if (elem)
            elem.scrollIntoView({ "block": "nearest" });
    }
}

export function elementIsVisible(element: HTMLElement): boolean {
    while (element) {
        if (window.getComputedStyle(element).display === 'none') {
            return false;
        }
        const parent = element.parentElement;
        if (!parent) break;
        element = parent;
    }
    return true;
}
