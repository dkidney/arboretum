
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arboretum

<!-- badges: start -->
<!-- badges: end -->

Plot customised phylogentic trees.

## Installation

You can install the latest version from [GitHub](https://github.com/)
with:

``` r
if (!requireNamespace("devtools")) install.packages('devtools')
devtools::install_github("dkidney/arboretum", ref="main")
```

## Examples

Load the package (and check the version)

``` r
library(arboretum)
packageVersion("arboretum")
#> [1] '0.2.0.9000'
```

Plot a tree for a specific using the `taxon` argument.

By default, any branch node ending in ‘-morpha’, ‘-formes’, ‘-oidea’,
‘-idae’, ‘-inae’, ‘-ini’ or ‘-ina’ will be collapsed, unless you use the
`collapse` argument to specify which taxa you wish to collapse.

Here are some examples.

``` r
tree('tetrapodomorpha')
#> 1 root: tetrapodomorpha
#> 5 nodes
#> 16 tips
#> 2 collapsed:
#> - archegosauridae
#> - reptiliomorpha
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

------------------------------------------------------------------------

``` r
tree('reptiliomorpha', collapse=c('diapsida', 'therapsida'), xmin=-345, xmax=-252)
#> 1 root: reptiliomorpha
#> 12 nodes
#> 15 tips
#> 2 collapsed:
#> - diapsida
#> - therapsida
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

------------------------------------------------------------------------

``` r
tree('therapsida', xmax=-200)
#> 1 root: therapsida
#> 6 nodes
#> 18 tips
#> 1 collapsed:
#> - mammaliamorpha
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

------------------------------------------------------------------------

``` r
tree('ornithischia', collapse='none')
#> 1 root: ornithischia
#> 12 nodes
#> 40 tips
#> 0 collapsed
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

------------------------------------------------------------------------

``` r
tree('sauropoda', collapse='none')
#> 1 root: sauropoda
#> 6 nodes
#> 18 tips
#> 0 collapsed
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

------------------------------------------------------------------------

``` r
tree('tyrannosauroidea', collapse='avialae', xmin=-173)
#> 1 root: tyrannosauroidea
#> 3 nodes
#> 18 tips
#> 0 collapsed
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />
