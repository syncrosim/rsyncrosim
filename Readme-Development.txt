GUIDE TO INSTALLING OR MODIFYING RSYNCROSIM - DEVELOPMENT VERSION
========================================================

***********************************
TO VIEW THE CODE ON GITHUB
***********************************
1) If you haven't done so, get a github account and ask Leonardo to add you to the ApexRMS organization. 
2) Log in to github. 
3) Find development code in the R directory at https://github.com/ApexRMS/dev.rsyncrosim. Eventually the 
master repository (https://github.com/ApexRMS/rsyncrosim) will give public access to the package - for now we will 
use the development branch.

**************************
TO EDIT THE CODE ON GITHUB
**************************
1) An ApexRMS administrator must add you to the rsyncrosim development team or make you a collaborator.
2) Note that the NAMESPACE and package documentation (.Rda) files are generated automatically from "' comments 
in the .R files using roxygen2. Direct modifications of the NAMESPACE and .Rda files will be lost. 
To revise the documentation edit the "' comments.

*************************************************
TO INSTALL THE DEVELOPMENT VERSION OF THE PACKAGE
*************************************************
1) Make sure you can read the repository: https://github.com/ApexRMS/dev.rsyncrosim
2) Get an access token (PAT) with repo scope: https://github.com/settings/tokens
3) In R: 
   install.packages(devtools)
   devtools::install_github("ApexRMS/dev.rsyncrosim",ref="dev",auth_token=yourPAT)
   library(rsyncrosim)

*******************************************
TO CHECK OUT A LOCAL COPY OF THE REPOSITORY
*******************************************
NOTE: This is only necessary if you anticipate doing a lot of work on the code. For minor changes, a simpler 
(and safer) solution is to edit the files on github. Users with a local copy and write privileges must take 
responsibility for using git properly.
1) Set up Git: https://help.github.com/articles/set-up-git/
2) Setup Git on RStudio and Associate with GitHub: https://www.r-bloggers.com/rstudio-and-github/
3) Clone the GitHub project to new RStudio project: https://www.r-bloggers.com/rstudio-and-github/. 
The repository url is https://[username]:[password]@github.com/ApexRMS/rsyncrosim.git and the remote origin 
url is git@github.com:ApexRMS/rsyncrosim.git 
4) Add the development branch. In the git bash:
   git checkout -b dev
   git remote add private git@github.com:ApexRMS/dev.rsyncrosim.git
   git pull private dev
   git push -u private dev
5) Use the RStudio GUI to commit and push changes to github. Please ensure you are committing to the dev branch, not the master.

