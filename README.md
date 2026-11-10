# cbbstats – Statistics Teaching at CBB

Quarto website for the statistics courses of the Campus Research Alliance Cognition, Brain, and Behavior (CBB), Göttingen:
*Linear Models in R*, *Open, Replicable, and Ethical Science* (ORES), *Testing for Absence and Equivalence* (TAE), short workshops, consultation and a glossary.

Live site: <https://simonstephan31.github.io/cbbstats/>

## Work on it in RStudio

1. Open `course-website-hub.Rproj`.
2. Edit the `.qmd` files (one folder per course: `linear-models/` with `sessions/`, `ores/`, `tae/`, `ibcm/`, plus `workshops/`).
3. Preview: **Build ▸ Render Website** (or `quarto preview` in the Terminal). The rendered site goes to `docs/`.

`freeze: auto` stores computed results in `_freeze/`, so code is only re-run for pages you change. Commit `_freeze/` too.
Re-rendering the Bayesian workshop page requires `brms` and a Stan backend; all other pages need `lme4`, `ggplot2`, `patchwork` and `scales`.

## Publish

Render, then commit and push everything, including `docs/` and `_freeze/`. GitHub Pages serves the site from `main` / `docs`
(*Settings ▸ Pages ▸ Build and deployment ▸ Deploy from a branch ▸ main ▸ /docs*).

## Adding materials

- Slides (Quarto revealjs, `embed-resources: true`) go to `slides/`, scripts to `scripts/`, data to `data/` (with an entry in `data/README.md`). Everything in these folders is published.
  These folders are copied into `docs/` on every render (see `resources:` in `_quarto.yml`).
- On a session page, replace the "coming soon" line with a link, e.g. `- [Slides](../../slides/11-glmm.html)`.
