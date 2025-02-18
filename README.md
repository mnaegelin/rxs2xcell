
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxs2xcell

<!-- badges: start -->
<!-- badges: end -->

The goal of rxs2xcell is to …

## Installation

You can install the development version of rxs2xcell from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mnaegelin/rxs2xcell")
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> 
#> → Will install 61 packages.
#> → Will update 1 package.
#> → Will download 61 CRAN packages (72.89 MB).
#> → Will download 1 package with unknown size.
#> + askpass                    1.2.1      🔧 ⬇ (25.15 kB)
#> + audio                      0.1-11     🔧 ⬇ (77.02 kB)
#> + aws.s3                     0.3.21      ⬇ (200.70 kB)
#> + aws.signature              0.6.0       ⬇ (77.64 kB)
#> + backports                  1.5.0      🔧 ⬇ (122.02 kB)
#> + base64enc                  0.1-3      🔧 ⬇ (34.80 kB)
#> + beepr                      2.0         ⬇ (1.06 MB)
#> + bit                        4.5.0.1    🔧 ⬇ (1.28 MB)
#> + bit64                      4.6.0-1    🔧 ⬇ (575.59 kB)
#> + checkmate                  2.3.2      🔧 ⬇ (778.25 kB)
#> + cli                        3.6.4      🔧 ⬇ (1.45 MB)
#> + clipr                      0.8.0       ⬇ (51.68 kB)
#> + colorspace                 2.1-1      🔧 ⬇ (2.67 MB)
#> + crayon                     1.5.3       ⬇ (164.41 kB)
#> + curl                       6.2.0      🔧 ⬇ (3.65 MB)
#> + data.table                 1.16.4     🔧 ⬇ (2.84 MB)
#> + digest                     0.6.37     🔧 ⬇ (356.05 kB)
#> + dplyr                      1.1.4      🔧 ⬇ (1.60 MB)
#> + exifr                      0.3.2       ⬇ (3.11 MB)
#> + fansi                      1.0.6      🔧 ⬇ (383.06 kB)
#> + farver                     2.1.2      🔧 ⬇ (1.97 MB)
#> + generics                   0.1.3       ⬇ (81.91 kB)
#> + ggnewscale                 0.5.0       ⬇ (352.71 kB)
#> + ggplot2                    3.5.1       ⬇ (4.97 MB)
#> + ggstance                   0.3.7       ⬇ (243.04 kB)
#> + glue                       1.8.0      🔧 ⬇ (173.70 kB)
#> + gtable                     0.3.6       ⬇ (224.61 kB)
#> + hms                        1.1.3       ⬇ (100.42 kB)
#> + httr                       1.4.7       ⬇ (478.86 kB)
#> + isoband                    0.2.7      🔧 ⬇ (1.87 MB)
#> + jsonlite                   1.8.9      🔧 ⬇ (1.13 MB)
#> + labeling                   0.4.3       ⬇ (61.49 kB)
#> + lifecycle                  1.0.4       ⬇ (124.78 kB)
#> + magrittr                   2.0.3      🔧 ⬇ (233.52 kB)
#> + mime                       0.12       🔧 ⬇ (37.02 kB)
#> + munsell                    0.5.1       ⬇ (246.54 kB)
#> + openssl                    2.3.2      🔧 ⬇ (3.87 MB)
#> + pillar                     1.10.1      ⬇ (657.46 kB)
#> + pkgconfig                  2.0.3       ⬇ (18.45 kB)
#> + plyr                       1.8.9      🔧 ⬇ (1.02 MB)
#> + purrr                      1.0.4      🔧 ⬇ (564.18 kB)
#> + R6                         2.6.1       ⬇ (86.91 kB)
#> + rappdirs                   0.3.3      🔧 ⬇ (47.90 kB)
#> + RColorBrewer               1.1-3       ⬇ (53.32 kB)
#> + Rcpp                       1.0.14     🔧 ⬇ (3.36 MB)
#> + readr                      2.1.5      🔧 ⬇ (1.97 MB)
#> + rlang                      1.1.5      🔧 ⬇ (1.90 MB)
#> + rxs2xcell     0.0.0.9000 → 0.0.0.9000 👷🏽‍♂️🔧 ⬇ (GitHub: a9b26ca)
#> + scales                     1.3.0       ⬇ (710.41 kB)
#> + stringi                    1.8.4      🔧 ⬇ (14.77 MB)
#> + stringr                    1.5.1       ⬇ (314.27 kB)
#> + sys                        3.4.3      🔧 ⬇ (51.49 kB)
#> + tibble                     3.2.1      🔧 ⬇ (688.89 kB)
#> + tidyr                      1.3.1      🔧 ⬇ (1.32 MB)
#> + tidyselect                 1.2.1       ⬇ (224.68 kB)
#> + tzdb                       0.4.0      🔧 ⬇ (1.24 MB)
#> + utf8                       1.2.4      🔧 ⬇ (206.91 kB)
#> + vctrs                      0.6.5      🔧 ⬇ (1.89 MB)
#> + viridisLite                0.4.2       ⬇ (1.30 MB)
#> + vroom                      1.6.5      🔧 ⬇ (3.08 MB)
#> + withr                      3.0.2       ⬇ (222.97 kB)
#> + xml2                       1.3.6      🔧 ⬇ (518.05 kB)
#> ℹ Getting 61 pkgs (72.89 MB) and 1 pkg with unknown size
#> ✔ Cached copy of rxs2xcell 0.0.0.9000 (source) is the latest build
#> ✔ Got askpass 1.2.1 (aarch64-apple-darwin20) (25.15 kB)
#> ✔ Got RColorBrewer 1.1-3 (aarch64-apple-darwin20) (53.20 kB)
#> ✔ Got R6 2.6.1 (aarch64-apple-darwin20) (86.91 kB)
#> ✔ Got audio 0.1-11 (aarch64-apple-darwin20) (76.94 kB)
#> ✔ Got aws.s3 0.3.21 (aarch64-apple-darwin20) (200.70 kB)
#> ✔ Got crayon 1.5.3 (aarch64-apple-darwin20) (163.86 kB)
#> ✔ Got generics 0.1.3 (aarch64-apple-darwin20) (79.87 kB)
#> ✔ Got checkmate 2.3.2 (aarch64-apple-darwin20) (778.25 kB)
#> ✔ Got httr 1.4.7 (aarch64-apple-darwin20) (478.86 kB)
#> ✔ Got colorspace 2.1-1 (aarch64-apple-darwin20) (2.66 MB)
#> ✔ Got fansi 1.0.6 (aarch64-apple-darwin20) (381.08 kB)
#> ✔ Got digest 0.6.37 (aarch64-apple-darwin20) (355.39 kB)
#> ✔ Got bit64 4.6.0-1 (aarch64-apple-darwin20) (575.59 kB)
#> ✔ Got jsonlite 1.8.9 (aarch64-apple-darwin20) (1.13 MB)
#> ✔ Got mime 0.12 (aarch64-apple-darwin20) (36.76 kB)
#> ✔ Got Rcpp 1.0.14 (aarch64-apple-darwin20) (3.35 MB)
#> ✔ Got gtable 0.3.6 (aarch64-apple-darwin20) (224.61 kB)
#> ✔ Got pkgconfig 2.0.3 (aarch64-apple-darwin20) (18.27 kB)
#> ✔ Got lifecycle 1.0.4 (aarch64-apple-darwin20) (124.15 kB)
#> ✔ Got stringr 1.5.1 (aarch64-apple-darwin20) (314.27 kB)
#> ✔ Got scales 1.3.0 (aarch64-apple-darwin20) (710.41 kB)
#> ✔ Got exifr 0.3.2 (aarch64-apple-darwin20) (3.11 MB)
#> ✔ Got purrr 1.0.4 (aarch64-apple-darwin20) (564.18 kB)
#> ✔ Got beepr 2.0 (aarch64-apple-darwin20) (1.06 MB)
#> ✔ Got cli 3.6.4 (aarch64-apple-darwin20) (1.45 MB)
#> ✔ Got tzdb 0.4.0 (aarch64-apple-darwin20) (1.24 MB)
#> ✔ Got bit 4.5.0.1 (aarch64-apple-darwin20) (1.28 MB)
#> ✔ Got backports 1.5.0 (aarch64-apple-darwin20) (121 kB)
#> ✔ Got clipr 0.8.0 (aarch64-apple-darwin20) (51.68 kB)
#> ✔ Got xml2 1.3.6 (aarch64-apple-darwin20) (518.40 kB)
#> ✔ Got ggstance 0.3.7 (aarch64-apple-darwin20) (243.04 kB)
#> ✔ Got ggnewscale 0.5.0 (aarch64-apple-darwin20) (352.71 kB)
#> ✔ Got tidyr 1.3.1 (aarch64-apple-darwin20) (1.33 MB)
#> ✔ Got rappdirs 0.3.3 (aarch64-apple-darwin20) (47.45 kB)
#> ✔ Got hms 1.1.3 (aarch64-apple-darwin20) (100.42 kB)
#> ✔ Got pillar 1.10.1 (aarch64-apple-darwin20) (657.46 kB)
#> ✔ Got viridisLite 0.4.2 (aarch64-apple-darwin20) (1.30 MB)
#> ✔ Got munsell 0.5.1 (aarch64-apple-darwin20) (243.41 kB)
#> ✔ Got tibble 3.2.1 (aarch64-apple-darwin20) (688.89 kB)
#> ✔ Got aws.signature 0.6.0 (aarch64-apple-darwin20) (77.55 kB)
#> ✔ Got dplyr 1.1.4 (aarch64-apple-darwin20) (1.60 MB)
#> ✔ Got labeling 0.4.3 (aarch64-apple-darwin20) (60.51 kB)
#> ✔ Got utf8 1.2.4 (aarch64-apple-darwin20) (206.43 kB)
#> ✔ Got sys 3.4.3 (aarch64-apple-darwin20) (51.50 kB)
#> ✔ Got vroom 1.6.5 (aarch64-apple-darwin20) (3.08 MB)
#> ✔ Got rlang 1.1.5 (aarch64-apple-darwin20) (1.89 MB)
#> ✔ Got base64enc 0.1-3 (aarch64-apple-darwin20) (34.41 kB)
#> ✔ Got magrittr 2.0.3 (aarch64-apple-darwin20) (233.32 kB)
#> ✔ Got data.table 1.16.4 (aarch64-apple-darwin20) (2.83 MB)
#> ✔ Got readr 2.1.5 (aarch64-apple-darwin20) (1.97 MB)
#> ✔ Got withr 3.0.2 (aarch64-apple-darwin20) (221.78 kB)
#> ✔ Got openssl 2.3.2 (aarch64-apple-darwin20) (3.87 MB)
#> ✔ Got glue 1.8.0 (aarch64-apple-darwin20) (173.00 kB)
#> ✔ Got tidyselect 1.2.1 (aarch64-apple-darwin20) (224.68 kB)
#> ✔ Got ggplot2 3.5.1 (aarch64-apple-darwin20) (4.97 MB)
#> ✔ Got farver 2.1.2 (aarch64-apple-darwin20) (1.97 MB)
#> ✔ Got isoband 0.2.7 (aarch64-apple-darwin20) (1.87 MB)
#> ✔ Got curl 6.2.0 (aarch64-apple-darwin20) (3.65 MB)
#> ✔ Got stringi 1.8.4 (aarch64-apple-darwin20) (14.75 MB)
#> ✔ Got plyr 1.8.9 (aarch64-apple-darwin20) (1.02 MB)
#> ✔ Got vctrs 0.6.5 (aarch64-apple-darwin20) (1.89 MB)
#> ✔ Installed rxs2xcell 0.0.0.9000 (github::mnaegelin/rxs2xcell@a9b26ca) (148ms)
#> ✔ Installed R6 2.6.1  (147ms)
#> ✔ Installed RColorBrewer 1.1-3  (147ms)
#> ✔ Installed askpass 1.2.1  (153ms)
#> ✔ Installed audio 0.1-11  (151ms)
#> ✔ Installed aws.s3 0.3.21  (150ms)
#> ✔ Installed aws.signature 0.6.0  (150ms)
#> ✔ Installed backports 1.5.0  (150ms)
#> ✔ Installed base64enc 0.1-3  (147ms)
#> ✔ Installed beepr 2.0  (146ms)
#> ✔ Installed Rcpp 1.0.14  (256ms)
#> ✔ Installed bit64 4.6.0-1  (175ms)
#> ✔ Installed bit 4.5.0.1  (46ms)
#> ✔ Installed checkmate 2.3.2  (33ms)
#> ✔ Installed cli 3.6.4  (35ms)
#> ✔ Installed clipr 0.8.0  (52ms)
#> ✔ Installed colorspace 2.1-1  (43ms)
#> ✔ Installed crayon 1.5.3  (41ms)
#> ✔ Installed curl 6.2.0  (34ms)
#> ✔ Installed data.table 1.16.4  (63ms)
#> ✔ Installed digest 0.6.37  (52ms)
#> ✔ Installed dplyr 1.1.4  (38ms)
#> ✔ Installed fansi 1.0.6  (19ms)
#> ✔ Installed exifr 0.3.2  (75ms)
#> ✔ Installed farver 2.1.2  (35ms)
#> ✔ Installed generics 0.1.3  (35ms)
#> ✔ Installed ggnewscale 0.5.0  (58ms)
#> ✔ Installed ggplot2 3.5.1  (38ms)
#> ✔ Installed ggstance 0.3.7  (37ms)
#> ✔ Installed glue 1.8.0  (34ms)
#> ✔ Installed gtable 0.3.6  (35ms)
#> ✔ Installed hms 1.1.3  (33ms)
#> ✔ Installed httr 1.4.7  (49ms)
#> ✔ Installed isoband 0.2.7  (49ms)
#> ✔ Installed jsonlite 1.8.9  (32ms)
#> ✔ Installed labeling 0.4.3  (30ms)
#> ✔ Installed lifecycle 1.0.4  (31ms)
#> ✔ Installed magrittr 2.0.3  (52ms)
#> ✔ Installed mime 0.12  (31ms)
#> ✔ Installed munsell 0.5.1  (30ms)
#> ✔ Installed openssl 2.3.2  (32ms)
#> ✔ Installed pillar 1.10.1  (33ms)
#> ✔ Installed pkgconfig 2.0.3  (30ms)
#> ✔ Installed plyr 1.8.9  (30ms)
#> ✔ Installed purrr 1.0.4  (32ms)
#> ✔ Installed rappdirs 0.3.3  (32ms)
#> ✔ Installed readr 2.1.5  (33ms)
#> ✔ Installed rlang 1.1.5  (33ms)
#> ✔ Installed scales 1.3.0  (51ms)
#> ✔ Installed stringr 1.5.1  (21ms)
#> ✔ Installed stringi 1.8.4  (70ms)
#> ✔ Installed sys 3.4.3  (31ms)
#> ✔ Installed tibble 3.2.1  (32ms)
#> ✔ Installed tidyr 1.3.1  (33ms)
#> ✔ Installed tidyselect 1.2.1  (34ms)
#> ✔ Installed tzdb 0.4.0  (34ms)
#> ✔ Installed utf8 1.2.4  (33ms)
#> ✔ Installed vctrs 0.6.5  (33ms)
#> ✔ Installed viridisLite 0.4.2  (32ms)
#> ✔ Installed vroom 1.6.5  (54ms)
#> ✔ Installed withr 3.0.2  (33ms)
#> ✔ Installed xml2 1.3.6  (21ms)
#> ✔ 1 pkg + 66 deps: kept 2, upd 1, added 61, dld 61 (72.83 MB) [16.4s]
```

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(rxs2xcell)
## basic example code

# TODO:
# write a use_template() function that takes a template file and opens it
# maybe just a wrapper around:
# usethis::use_template('template_workflow.Rmd', package='rxs2xcell')
# maybe something like the use_targets() function from https://docs.ropensci.org/targets/reference/use_targets.html
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
