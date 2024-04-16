# Osteometric Analysis of Mammalian Fauna from the Late Epipalaeolithic and Early Neolithic Azraq Basin

Joe Roe, University of Bern  
Lisa Yeomans, University of Copenhagen  
Louise Martin, University College London

This [research compendium](https://research-compendium.science/) contains the data and code for an osteometric analysis of faunal remains (*Gazella sp.*, *Lepus sp.*, and  *Vulpes sp.*) from Late Epipalaeolithic and Early Neolithic archaeological sites in the eastern Jordanian *badia*, c. 25 – 7.5 ka. It accompanies our paper:

> Martin, L., Roe, J. and Yeomans, L. 2024. Late Pleistocene-Holocene mammalian body size change in Jordan’s Azraq Basin: a case for climate driven species distribution shifts. Paper submitted for publication.

## Usage

The compendium is provided as an R package.
The main analysis is described in `analysis/`, in the series of R scripts starting `0-data.R`, using raw data from `analysis/data`.
Additional R functions are defined in `R/`.

To reproduce the analysis in full:

1. Download or clone the latest version of this repository
2. Build and install the package with `devtools::install(dependencies = TRUE)` (shortcut `Ctrl+Shift+B` in RStudio)
3. Source each .R file in `analysis/` in sequence

The figures included in the paper are generated from `3-figures.R` and can be found in `figures/` (apart from figure 1, which was made in QGIS).
`6-suppl-figures.Rmd` is an RMarkdown document that generates the file with our supplementary figures.
