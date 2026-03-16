# Installing GitHub release

Follow these instructions to install the latest `rsyncrosim` release
from GitHub.

1.  Open RStudio (or any R IDE)

2.  Make sure that you do not have another version of the `rsyncrosim`
    library loaded in your R session. You can detach a loaded library
    using the following code:

``` r
detach("package:rsyncrosim", unload=TRUE)
```

3.  Use the
    [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
    function to install `rsyncrosim` from a website URL. Change the
    `X.X.X` in the code below to your desired version, then run the code
    to install that version from the `rsyncrosim` GitHub repository:

``` r
# Change the line below to your desired rsyncrosim version
rsyncrosimVersion = "X.X.X"

install.packages(
  paste0("https://github.com/syncrosim/rsyncrosim/releases/download/", 
         rsyncrosimVersion, 
         "/rsyncrosim_", 
         rsyncrosimVersion, 
         ".tar.gz"), repo=NULL)
```

4.  You many need to restart your R environment to get rid of any
    environmental conflicts. You can do this from RStudio using the
    following code:

``` r
.rs.restartR()
```

5.  Load the `rsyncrosim` library.

``` r
library(rsyncrosim)
```
