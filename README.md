# Experiment code

This repository contains the code used to generate the simulation results, indices, and figures for the paper.

## Requirements
- GNU Make
- R (and packages loaded by `01_loading.R`, e.g. tidyverse, data.table, sf, spdep, foreach/doSNOW, argparse, etc.)

## Reproducing results
From the project root:

- **Default (recommended for review):**
  ```sh
  make all
  ```
  Generates the random simulation outputs (including **GINI**) and then computes indices.
  It **does not** re-run the expensive adversarial fit (`03_fit_adv.R`).

- **Adversarial downstream outputs (without re-fitting):**
  ```sh
  make advs
  ```
  This requires a precomputed `data/results_adv.Rdata`.

- **To regenerate the expensive adversarial fit explicitly:**
  ```sh
  make fit_adv
  ```

- **To regenerate random fits explicitly:**
  ```sh
  make fit_random
  make fit_random_gini
  ```

## Notes
- The build expects precomputed `data/results_adv.Rdata` to be present for `make all`/`make advs` (or generate it with `make fit_adv`).
- Scripts are runnable from any working directory (no hardcoded `~/workflow` paths).
