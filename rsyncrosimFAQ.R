#################################
# Problem: No help when package is reinstalled.
# Solution: restart R
# Minimal example:
   devtools::install_github("ApexRMS/rsyncrosim")
   library(rsyncrosim)
   ?session
   ?session
   devtools::install_github("ApexRMS/rsyncrosim")
   library(rsyncrosim)
   ?session
   ?session
# Warning message: In fetch(key) : internal error -3 in R_decompress1
# Discussion:
# This is a known problem with R base - when you use help in a package the help database is cached.
# https://github.com/hadley/devtools/issues/419
# https://stat.ethz.ch/pipermail/r-help/2011-July/283916.html