
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mop

<!-- badges: start -->
<!-- badges: end -->

Data from lab instruments are often consistently untidy, and can
therefore be programmatically tidied. This package seeks to tidy lab
data much like how `broom` tidies statistical objects, with a couple
important differences:

1.  Each data type must be read in, and has its own `read_*` function.
    This can’t really be avoided: there is really no practical way to
    provide a single function that can accurately and consistently
    determine the source of a given file.
2.  Outputs from `tidy_lab` are *not* `data.frame`s. They are instead
    objects that *contain* tidy data. This allows for packages to
    interact with these downstream objects in unique ways depending on
    the data type (ie, making generic functions that utilize these
    objects). If you would prefer a tidy `data.frame` (or more
    specifically, `tibble`), run `scrub` on the object.

## Installation

This package can be downloaded from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KaiAragaki/mop")
```

## Basic Workflow

Using `mop` generally begins with reading in your lab data with it’s
respective function. For example, a nanodrop file:

``` r
library(mop)

nano <- system.file("extdata", "nanodrop.csv", package = "mop") |>
  read_nanodrop(nucleotide = "RNA", date = "2021-08-14")

nano
#> <nanodrop[5]>
#> # A tibble: 18 × 24
#>    Date            Sample.Name Nucleic.Acid.ng.… A260.A280 A260.A230  A260  A280
#>    <chr>           <chr>                   <dbl>     <dbl>     <dbl> <dbl> <dbl>
#>  1 8/14/2021 8:26… Sample 1                 420.      2.03      2.29 10.5   5.16
#>  2 8/14/2021 8:27… Sample 2                 450.      2.06      2.26 11.2   5.46
#>  3 8/14/2021 8:28… Sample 3                 498.      2.06      2.28 12.4   6.03
#>  4 8/14/2021 8:28… Sample 4                 449.      2.05      2.25 11.2   5.48
#>  5 8/14/2021 8:29… Sample 5                 474.      2.03      2.29 11.9   5.85
#>  6 8/14/2021 8:29… Sample 6                 543.      2.00      2.17 13.6   6.80
#>  7 8/14/2021 8:30… Sample 7                 483.      2.07      2.24 12.1   5.84
#>  8 8/14/2021 8:31… Sample 8                 588.      2.07      1.97 14.7   7.08
#>  9 8/14/2021 8:31… Sample 9                 490.      2.07      2.25 12.2   5.91
#> 10 8/14/2021 8:32… Sample 10                256.      2.03      2.27  6.40  3.15
#> 11 8/14/2021 8:32… Sample 11                225.      2.03      2.27  5.62  2.77
#> 12 8/14/2021 8:33… Sample 12                429.      2.06      2.26 10.7   5.22
#> 13 8/14/2021 8:35… Sample 13                216.      2.02      2.27  5.39  2.66
#> 14 8/14/2021 8:35… Sample 14                218.      2.03      2.29  5.44  2.68
#> 15 8/14/2021 8:36… Sample 15                206.      2.03      2.24  5.15  2.54
#> 16 8/14/2021 8:37… Sample 16                426.      2.07      2.25 10.7   5.15
#> 17 8/14/2021 8:37… Sample 17                389.      2.06      2.28  9.74  4.73
#> 18 8/14/2021 8:38… Sample 18                560.      2.04      2.19 14.0   6.88
#> # … with 17 more variables: Nucleic.Acid.Factor <dbl>,
#> #   Baseline.Correction..nm. <int>, Baseline.Absorbance <dbl>,
#> #   Corrected..ng.uL. <lgl>, Corrected..CV <lgl>, Impurity.1 <lgl>,
#> #   Impurity.1.A260 <lgl>, Impurity.1..CV <lgl>, Impurity.1.mM <lgl>,
#> #   Impurity.2 <lgl>, Impurity.2.A260 <lgl>, Impurity.2..CV <lgl>,
#> #   Impurity.2.mM <lgl>, Impurity.3 <lgl>, Impurity.3.A260 <lgl>,
#> #   Impurity.3..CV <lgl>, Impurity.3.mM <lgl>
#> # Nucelotide: RNA 
#> # Is tidy: FALSE 
#> # Date: 2021-08-14
```

Somethings to note:

-   `nano` is currently NOT tidy
-   `nano` is a `nanodrop` object

Additionally, `read_nanodrop` will normally try to extract nucleotide
and date information from the file name, but were supplied manually here
as the file is named to be understandable.

To tidy any lab object, pass it to `tidy_lab`:

``` r
nano_tidy <- tidy_lab(nano)
nano_tidy
#> <nanodrop[5]>
#> # A tibble: 18 × 24
#>    date        sample_name  conc a260_280 a260_230  a260  a280 nucleic_acid_fac…
#>    <chr>       <chr>       <dbl>    <dbl>    <dbl> <dbl> <dbl>             <dbl>
#>  1 2021-08-14… Sample 1     420.     2.03     2.29 10.5   5.16                40
#>  2 2021-08-14… Sample 2     450.     2.06     2.26 11.2   5.46                40
#>  3 2021-08-14… Sample 3     498.     2.06     2.28 12.4   6.03                40
#>  4 2021-08-14… Sample 4     449.     2.05     2.25 11.2   5.48                40
#>  5 2021-08-14… Sample 5     474.     2.03     2.29 11.9   5.85                40
#>  6 2021-08-14… Sample 6     543.     2.00     2.17 13.6   6.80                40
#>  7 2021-08-14… Sample 7     483.     2.07     2.24 12.1   5.84                40
#>  8 2021-08-14… Sample 8     588.     2.07     1.97 14.7   7.08                40
#>  9 2021-08-14… Sample 9     490.     2.07     2.25 12.2   5.91                40
#> 10 2021-08-14… Sample 10    256.     2.03     2.27  6.40  3.15                40
#> 11 2021-08-14… Sample 11    225.     2.03     2.27  5.62  2.77                40
#> 12 2021-08-14… Sample 12    429.     2.06     2.26 10.7   5.22                40
#> 13 2021-08-14… Sample 13    216.     2.02     2.27  5.39  2.66                40
#> 14 2021-08-14… Sample 14    218.     2.03     2.29  5.44  2.68                40
#> 15 2021-08-14… Sample 15    206.     2.03     2.24  5.15  2.54                40
#> 16 2021-08-14… Sample 16    426.     2.07     2.25 10.7   5.15                40
#> 17 2021-08-14… Sample 17    389.     2.06     2.28  9.74  4.73                40
#> 18 2021-08-14… Sample 18    560.     2.04     2.19 14.0   6.88                40
#> # … with 16 more variables: baseline_correction_nm <int>,
#> #   baseline_absorbance <dbl>, corrected_ngul <lgl>, corrected_cv <lgl>,
#> #   impurity_1 <lgl>, impurity_1_a260 <lgl>, impurity_1_cv <lgl>,
#> #   impurity_1_m_m <lgl>, impurity_2 <lgl>, impurity_2_a260 <lgl>,
#> #   impurity_2_cv <lgl>, impurity_2_m_m <lgl>, impurity_3 <lgl>,
#> #   impurity_3_a260 <lgl>, impurity_3_cv <lgl>, impurity_3_m_m <lgl>
#> # Nucelotide: RNA 
#> # Is tidy: TRUE 
#> # Date: 2021-08-14
```

Of note, our raw data is stored in `nano$raw_data`.

The operations that make an object tidy vary per object class, and can
be found in the object’s documentation (here `?tidy_lab.nanodrop`).

Objects are useful as they form a semi-stable language for other
functions from this and other packages to operate on. However, it’s
often much simpler to interact with data in a flat `tibble`. This can be
done using `scrub`:

``` r
nano_scrub <- scrub(nano_tidy)

nano_scrub[c(1:3, (ncol(nano_scrub)-2):ncol(nano_scrub))]
#> # A tibble: 18 × 6
#>    date                sample_name  conc exp_date   nucleotide is_tidy
#>    <chr>               <chr>       <dbl> <date>     <chr>      <lgl>  
#>  1 2021-08-14 20:26:49 Sample 1     420. 2021-08-14 RNA        TRUE   
#>  2 2021-08-14 20:27:25 Sample 2     450. 2021-08-14 RNA        TRUE   
#>  3 2021-08-14 20:28:07 Sample 3     498. 2021-08-14 RNA        TRUE   
#>  4 2021-08-14 20:28:40 Sample 4     449. 2021-08-14 RNA        TRUE   
#>  5 2021-08-14 20:29:17 Sample 5     474. 2021-08-14 RNA        TRUE   
#>  6 2021-08-14 20:29:54 Sample 6     543. 2021-08-14 RNA        TRUE   
#>  7 2021-08-14 20:30:30 Sample 7     483. 2021-08-14 RNA        TRUE   
#>  8 2021-08-14 20:31:16 Sample 8     588. 2021-08-14 RNA        TRUE   
#>  9 2021-08-14 20:31:50 Sample 9     490. 2021-08-14 RNA        TRUE   
#> 10 2021-08-14 20:32:24 Sample 10    256. 2021-08-14 RNA        TRUE   
#> 11 2021-08-14 20:32:59 Sample 11    225. 2021-08-14 RNA        TRUE   
#> 12 2021-08-14 20:33:50 Sample 12    429. 2021-08-14 RNA        TRUE   
#> 13 2021-08-14 20:35:16 Sample 13    216. 2021-08-14 RNA        TRUE   
#> 14 2021-08-14 20:35:59 Sample 14    218. 2021-08-14 RNA        TRUE   
#> 15 2021-08-14 20:36:34 Sample 15    206. 2021-08-14 RNA        TRUE   
#> 16 2021-08-14 20:37:12 Sample 16    426. 2021-08-14 RNA        TRUE   
#> 17 2021-08-14 20:37:50 Sample 17    389. 2021-08-14 RNA        TRUE   
#> 18 2021-08-14 20:38:21 Sample 18    560. 2021-08-14 RNA        TRUE
```

Note how meta-data fields have now become individual columns.
