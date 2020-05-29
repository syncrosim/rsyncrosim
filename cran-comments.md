## Test environments
* Windows 10 (local 4.0 // CI (GitHub actions), release 3.6.3 and devel 4.0)
* Pop! OS 19.04 (local, 3.6.2, docker container 4.0)
* Ubuntu 18.04 (CI (GitHub actions), release 3.6.3 and devel 4.0)

## Resubmission
This is a resubmission. 
In version 1.2.3 we addressed all the comments provided sucessively by by Uwe Ligges, 
Martina Schmirl and Jelena Saf :

* Following a suggestion by Martina Schimrl, the examples are wrapped in \\donttest{}
instead of \\donttrun{]. They cannot be unwrapped as they will fail on testing platforms given that they require SyncroSim to work (see the Upstream dependencies Section below).
  * ** This is likely to cause R CMD check to produce an error on the CRAN servers 
  as in R 4.0, CMD check now runs examples wrapped in \\donttest{} **

* Following a comment by Martina Schmirl, the behavior of functions that returned 
strings and printed them has been modified to use message(). These messages are 
important to keep for the user to obtain critical information about their 
interaction with the Syncrosim API and should not be suppressed. Those functions now 
returns a boolean invisibly.

* Following a comment by Martina Schmirl, all default path have been removed and 
examples/vignette code uses tempdir() and nothing is written to the user working directory, 
except in the default use of `ssimLibrary` which much like functions like `write.csv`
will create a SyncroSim library into the user working directory if they do not 
specify a path.

Smaller changes:
* Descripton file have been updated with undirected quotation marks
* The \\value field has been added/updated for all functions

## Upstream dependencies

The Syncrosim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires Syncrosim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all code from vignettes is 
set to not be evaluated wehn NOT_CRAN is FALSE is the environment.

## R CMD check results
Duration: 2m 20s

There was 1 note:

  Maintainer: 'Colin Daniel <colin.daniel@apexrms.com>'
  
  New submission

0 errors √ | 0 warnings √ | 1 note x