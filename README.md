# QuickPop
 
Tools to work quickly around population genetics analyses

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/biodray/QuickPop/workflows/R-CMD-check/badge.svg)](https://github.com/biodray/QuickPop/actions)
  <!-- badges: end -->

This package in intend to ease the repetition of simple task typical of population genetic pipeline. This is a work in progress. More functions and vignettes will be added through times.

## How to install this package

You can install directly the last version from github

```{r}
library(remotes)
remotes::install_github("biodray/QuickPop")
```

For windows user, it is possible that you get into problems. You can try to set this parameter before the installation.

```{r}
# Change the behaviours of remotes
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
```
