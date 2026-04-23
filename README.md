# Lakers Evolution Dashboard

An interactive dashboard exploring how the Los Angeles Lakers' shot selection and efficiency have changed over recent seasons.

**Live dashboard:** <https://YOUR-USERNAME.github.io/nba-analytics/>

Built with R, Shiny, and `bslib`, published to GitHub Pages via [Shinylive](https://shinylive.io/r/) — the app runs entirely in the visitor's browser, with no server.

## Data

Shot-level data pulled from the NBA Stats API via the [`hoopR`](https://hoopr.sportsdataverse.org/) package.

## Repository layout

-   `app/` — the Shiny app (`app.R` and `lakers_shots.rds`)
-   `docs/` — the Shinylive build served by GitHub Pages
-   `data/` — raw and cleaned data pulled from the API
