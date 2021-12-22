
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mop

<!-- badges: start -->
<!-- badges: end -->

Data from lab instruments are often not tidy, but not tidy in a
consistent way. Because of this, we can programmatically tidy them. This
package seeks to tidy lab data much like how `broom` tidies statistical
objects, with a couple important differences:

1.  Each data type must be read in, and has its own `read_*` function.
    This can’t really be avoided: there is really no practical way to
    provide a single function that can accurately and consistently
    determine the source of a given file.
2.  Outputs from `tidy_lab` are *not* `data.frame`s. They are instead
    objects that *contain* tidy data. This allows for packages to
    interact with these downstream objects in unique ways depending on
    the data type (ie, making generic functions that utilize these
    objects). If you would prefer a tidy `data.frame` (or more
    specifically, `tibble`), either run `scrub` on the tidied object, or
    set `scrub = TRUE` in `tidy_lab`.

## Installation

This package can be downloaded from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KaiAragaki/mop")
```

## Example

``` r
library(mop)
## basic example code
```
