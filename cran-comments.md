## Test environments
* Windows 10 (local 4.0 // CI (GitHub actions), release 3.6.3 and devel 4.0)
* Pop! OS 19.04 (local, 3.6.2, docker container 4.0)
* Ubuntu 18.04 (CI (GitHub actions), release 3.6.3 and devel 4.0)

## Resubmission
This is a resubmission. 
Tn this version we have addressed all the comments provided by Martina Schmirl:

* License file has been replaced
* Package name formatting has been updated
* Version requirements have been clarified
* Links to the API and its documentation have been added with the right format (i.e. <"">)
* The \value field has been added/updated for all functions
* All default path have been removed and examples/vignette code uses tempdir()
* All \donteest{} instances have been replaced with \donttest{}
  * **This is likely to cause R CMD check to fail on the CRAN servers as in R 4.0, CMD check now runs examples wrapped in donttest, and this code requires SyncroSim installed (see the Upstream dependencies Section below).**
* The behavior of functions that returned strings and printed them has been modified to using message(). Those functions now returns a boolean invisibly.

## R CMD check results
Duration: 1m 57.8s

There was 1 note:

checking CRAN incoming feasibility ... NOTE
Maintainer: 'Colin Daniel <colin.daniel@apexrms.com>'
  
New submission

0 errors v | 0 warnings v | 1 note x

## Upstream dependencies

The Syncrosim software is an upstream dependency as rsyncrosim provides an API for it. Therefore, all examples, all tests, as well as vignette code, requires Syncrosim to be installed to run. Therefore, all tests in the submitted package should not run (tagged with testthat::skip_on_cran()). In addition, all code from vignettes is set to not be evaluated wehn NOT_CRAN is FALSE is the environment.