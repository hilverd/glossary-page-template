# Glossary Page Template Overview

This project provides a single HTML page with a built-in editor for creating a glossary that can be hosted anywhere.

## Dev environment tips

- Use `pnpm run build` to build the project.

## Testing instructions

- Use `pnpm exec elm-test` to run the Elm-based tests under `tests/`.
- Use `pnpm exec elm-verify-examples --run-tests` to verify examples in documentation strings.
- Fix any test or type errors until the whole suite is green.
- Add or update tests for the code you change, even if nobody asked.
