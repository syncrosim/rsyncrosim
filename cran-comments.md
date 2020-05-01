## Test environments
* Windows 10 (local 4.0 // CI (GitHub actions), release 3.6.3 and devel 4.0)
* Pop! OS 19.04 (local, 3.6.2, docker container 4.0)
* Ubuntu 18.04 (CI (GitHub actions), release 3.6.3 and devel 4.0)

## R CMD check results
Duration: 1m 57.8s

There was 1 note:

checking CRAN incoming feasibility ... NOTE
Maintainer: 'Colin Daniel <colin.daniel@apexrms.com>'
  
New submission

0 errors v | 0 warnings v | 1 note x

## Upstream dependencies

The Syncrosim software is an upstream dependency as rsyncrosim provides an API for it. Therefore, all tests, as well as vignette code, requires Syncrosim to be installed to run. Therefore, all tests in the submitted package should not run (tagged with testthat::skip_on_cran()). In addition, all code from vignettes is set to not be evaluated wehn NOT_CRAN is FALSE is the environment.