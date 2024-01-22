# Development

You'll need to have [pnpm](https://pnpm.io/) installed.

```
git clone https://github.com/hilverd/glossary-page-template.git
cd glossary-page-template
pnpm install
pnpm run dev
```

Then open http://localhost:3000/glossary.html in a browser.

## Building a release from sources

```
pnpm run build
./bin/prepare-release
```
