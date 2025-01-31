
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

Load the package (and check the version)

``` r
library(arboretum)
packageVersion("arboretum")
#> [1] '0.0.0.9000'
```

Plot the default tree. By default, any branch node ending in ‘-ae’,
‘-morpha’ or ‘-formes’ will be collapsed.

``` r
tree()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Use the `taxon` argument to create a tree for a specific taxon, using
the default rule for collapsing branches.

``` r
 tree(taxon = 'dinosauria')
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Use the `collapse` argument to choose which branches in the tree are
collapsed.

``` r
tree(taxon = 'dinosauria',
     collapse = c('sauropodomorpha',
                 'ornithopoda',
                 'theropoda',
                 'ankylosauria',
                 'stegosauria',
                 'pachycephalosauria',
                 'ceratopsia'))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Use `collapse='none'` to see all available branches.

``` r
tree(taxon='sauropoda', collapse='none')
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Other example trees:

``` r
tree('tetanurae', collapse='avialae')
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
tree('sauropterygia', collapse='none')
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
tree('synapsida')
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
tree('pseudosuchia', collapse='none')
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />
