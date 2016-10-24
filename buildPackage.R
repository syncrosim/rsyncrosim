#following http://r-pkgs.had.co.nz/intro.html
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

install.packages(devtools)
devtools::has_devel()

library(raster)
class ? RasterBrick
#########################
#For fast local development
#load package directly to memory without install
devtools::document()
devtools::load_all()

#Or build and reload in R studio.

#########################
#Install from github package source
#auth_token To install from a private repo, generate a personal
#'   access token (PAT) in \url{https://github.com/settings/tokens} and
#'   supply to this argument.

devtools::install_github("ApexRMS/dev.rsyncrosim",ref="dev",auth_token="29e830ebdf432c947be1a3a89cfa6c766233b10a")

##########################
#Merge development branch with master, then install from master
#When repo is public the auth_token is not required.
#From git shell
#git merge master
#git checkout master
#git merge dev
#git push origin master
devtools::install_github("ApexRMS/rsyncrosim",auth_token="29e830ebdf432c947be1a3a89cfa6c766233b10a")


#Build a binary package for users without development tools
devtools::build(binary = TRUE)
