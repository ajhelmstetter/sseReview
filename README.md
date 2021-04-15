<!-- README.md is generated from README.Rmd. Please edit that file -->

# sseReview

<!-- badges: start -->

[![R CMD
Check](https://github.com/ajhelmstetter/sseReview/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ajhelmstetter/sseReview/actions/workflows/R-CMD-check.yaml)
[![License: GPL (&gt;=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![Dependencies](https://img.shields.io/badge/dependencies-0/0-brightgreen?style=flat)](#)
<!-- badges: end -->

Research Compendium of the project **DiveRS -SSE review**

### How to cite

Please cite this compendium as:

> **{{ PLEASE ADD A CITATION }}**

### Content

This repository is structured as follow:

-   [`data/`](https://github.com/ajhelmstetter/sseReview/tree/master/data):
    contains all raw data required to perform analyses

-   [`rscripts/`](https://github.com/ajhelmstetter/sseReview/tree/master/rscripts/):
    contains R scripts to run each step of the workflow

-   [`outputs/`](https://github.com/ajhelmstetter/sseReview/tree/master/outputs):
    contains all the results created during the workflow

-   [`figures/`](https://github.com/ajhelmstetter/sseReview/tree/master/figures):
    contains all the figures created during the workflow

-   [`paper/`](https://github.com/ajhelmstetter/sseReview/tree/master/paper):
    contains all the manuscript and related content (biblio, templates,
    etc.)

-   [`R/`](https://github.com/ajhelmstetter/sseReview/tree/master/R):
    contains R functions developed especially for this project

-   [`man/`](https://github.com/ajhelmstetter/sseReview/tree/master/man):
    contains help files of R functions

-   [`DESCRIPTION`](https://github.com/ajhelmstetter/sseReview/tree/master/DESCRIPTION):
    contains project metadata (author, date, dependencies, etc.)

-   [`make.R`](https://github.com/ajhelmstetter/sseReview/tree/master/make.R):
    master R script to run the entire project by calling each R script
    stored in the `rscripts/` folder

### Usage

Clone the repository, open the `.Rproj` file in RStudio and run:

    source("make.R")

### Notes

-   All required packages, listed in the `DESCRIPTION` file, will be
    installed (if necessary)
-   All required packages and R functions will be loaded
-   Some analyses listed in the `make.R` might take time

### Plots

![](figures/stacked_barplot_models.png)

Figure 1. Stacked barplot of -SSE model use over time in angiosperms.
Bars represent total number of times different models are used. Multiple
different models can be taken from a single study, but if the same model
is used multiple times it is only counted once. Black line indicates the
number of studies published per year.

![](figures/ridgeplot_tips_year.png)

Figure 2. Ridgeplot of number of tips (log) in phylogenetic trees used
in -SSE model publications on angiosperm taxa. Density plots are shown
for each year separately showing a gradual increase in the number of
tips in the tree over time.

![](figures/ridgeplot_tips_model.png)

Figure 3. Ridgeplot of number of tips (log) in publications using -SSE
models in angiosperm taxa. Density plots are shown for each -SSE model
type separately, showing those model types that tend to be used with
higher numbers of tips.

![](figures/treemap_orders.png) Figure 4. Treemap of the orders
considered in angiosperm-based studies using -SSE methods.

![](figures/treemap_traits.png) Figure 5. Treemap of the trait types
considered in angiosperm-based studies using -SSE methods.

![](figures/raincloud_tips.png) Figure 6. Raincloud plots of
distributions of the number of tips in the phylogenetic trees in studies
that infer the an effect of the investigated trait (pink) or infer no
effect (green). On average more tips are found in studies that find an
association.

![](figures/densities.png) Figure 7. Paired density plots of
distributions of various factors in studies that infer the an effect of
the investigated trait (yellow) or infer no effect (grey). Older trees
and those with lower sampling fraction more often yield significant
associations.

![](figures/scatterplot_sampling_tips.png) Figure 8. Scatterplot of
sampling fraction against number of tips with fitted lines. Points and
lines coloured by whether there was a significant association (yellow)
or not (grey) between the traits and diversification rates. An almost
flat relationship observed between sampling fraction and number of tips,
and no effect between trait significance.
