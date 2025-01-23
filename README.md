
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arboretum

<!-- badges: start -->
<!-- badges: end -->

Plot customised phylogentic trees.

## Installation

You can install the development version of arboretum from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dkidney/arboretum")
```

## Examples

Plot the default tree.

``` r
library(arboretum)
tree() # default settings
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Create customised trees by selecting a specific taxon and/or collapsing
specific taxa.

``` r
tree(taxon='amniota', 
     collapse=c('archosauromorpha', 'therapsida', 'lepidosauromorpha'))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
tree(taxon='archosauromorpha', 
     collapse=c('dinosauria', 'pterosauria', 'pseudosuchia', 'sauropterygia'))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
tree('dinosauria', 
     collapse = c('sauropoda', 'ornithopoda', 'theropoda', 'ankylosauria', 'ceratopsia'))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
tree(taxon='tetanurae', collapse=c('avialae'))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
