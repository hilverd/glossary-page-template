# Glossary Page Template Overview

This project provides a single HTML page with a built-in editor for creating a glossary that can be hosted anywhere.

## Dev environment tips

- Use `pnpm run build` to build the project.

## Testing instructions

- Use `./node_modules/.bin/elm-test --compiler ./node_modules/.bin/elm` to run the Elm-based tests under `tests/`.
- Use `./node_modules/.bin/elm-verify-examples --compiler ./node_modules/.bin/elm --run-tests` to verify examples in documentation strings.
- Fix any test or type errors until the whole suite is green.
- Add or update tests for the code you change, even if nobody asked.
