# Repository Guidelines

## Project Structure & Module Organization
This repository is a Hakyll-based personal site. Core build logic lives in `src/site.hs`, with `src/News.hs` handling homepage news parsing. Content pages such as `index.md`, `blog.md`, and `news.md` sit at the root; dated articles live in `posts/`. Templates are in `templates/`, styles in `styles/`, bibliography files in `bib/`, and static assets in `images/`. Helper scripts live in `scripts/`, notably `scripts/math.ts` for KaTeX rendering. Treat `_site/`, `_cache/`, and `.stack-work/` as generated output. `slotThe.github.io/` is reference material and should not be changed unless a task explicitly targets it.

## Build, Test, and Development Commands
Use Stack for the Haskell build and site generation:

- `stack build` compiles the site generator.
- `stack exec site build` generates the site into `_site/`.
- `stack exec site clean` removes generated output.
- `stack exec site watch` rebuilds on changes and serves a local preview.
- `./start_dev_server.sh` runs the usual local loop: build, clean, then watch.

The build also depends on external tools used by `src/site.hs`: `sass` for `styles/main.scss` and `deno` for math rendering.

## Coding Style & Naming Conventions
Follow the existing Haskell style: `Haskell2010`, language pragmas at the top, and 2-space indentation for wrapped expressions. Keep modules small and focused; place reusable parsing or context logic in `src/` modules rather than expanding `Main` unnecessarily. Use descriptive names like `loadNewsEntries` and `postListContext`. Markdown content files use lowercase, hyphenated names such as `expected-information-as-expected-utility.md`.

## Testing Guidelines
There is no separate automated test suite in the root project. Validate changes by running `stack build` and `stack exec site build`; for content or template edits, also use `stack exec site watch` and inspect the affected page locally. If you change parsing logic, confirm both successful builds and expected rendered output.

## Commit & Pull Request Guidelines
Recent history uses short, imperative commit messages, for example `add tags` or `Cache Stack builds in Pages workflow`. Keep commits focused and use the same style. Pull requests should include a brief summary, note any content or template pages affected, link related issues when relevant, and include screenshots for visible UI or layout changes. Confirm the site builds locally before requesting review.
