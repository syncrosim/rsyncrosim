install.packages("devtools","roxygen2")

library(raster)

#getwd()
#dir.create(file.path(getwd(),"Temp"))
#setwd("Temp")
#setwd('..')

#create or load a library
libName = paste0(getwd(),"/Temp/ST-Sim-Command-Line")
myLibrary = ssimLibrary(model="st-sim",name=libName)
myLibrary
path(myLibrary)

