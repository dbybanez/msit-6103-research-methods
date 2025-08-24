# MSIT-6103 Research Methods — Code

This repo contains **code-only** mini-exercises for MSIT-6103.
Notes and slides are kept outside Git.

## Structure
- `exercises/` — one folder per exercise (R scripts + Quarto notebooks)
- `R/` — shared helper functions
- `data/` — small datasets used across exercises (optional)
- `scripts/` — utilities (e.g., new-exercise scaffold)
- `output/` — per-exercise outputs (gitignored)

## Getting Started
1. Install R, VS Code R extension, and restore libs:
   ```r
   install.packages("renv"); renv::restore()