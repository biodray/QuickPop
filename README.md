# QuickPop
 Tools to work around population genetics analyses

This is a work in progress. More functions and vignettes will be added through times...

## How to install this package
You can install directly the last version from github
```{r}
# Change the behaviours of remotes
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

# Load
library(remotes)
remotes::install_github("biodray/QuickPop")
```
## How to run basic functions

### Evanno

summary.str  <- data("structure")
str_evanno(summary.str)
