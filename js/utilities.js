export const untilAsync = async (fn, time = 1000, wait = 10000) => {
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

export function waitForElement(elementId) {
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