# QuickPop
 Tools to work around population genetics analyses

## How to install this package

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

library(devtools)
install_github("biodray/QuickPop")



## How to run basic functions


### Evanno

summary.str  <- data("structure")
compute.evanno(summary.str)
