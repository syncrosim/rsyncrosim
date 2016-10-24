
devtools::document();devtools::load_all()
#devtools::install_github("ApexRMS/dev.rsyncrosim",ref="dev",auth_token="29e830ebdf432c947be1a3a89cfa6c766233b10a")
#source("R/generics.R");source("R/ssimLibrary.R");source("R/session.R");source("R/command.R")
library(rsyncrosim)

?rsyncrosim
?command
?session
?Session
?filepath
?info
?SSimLibrary
?ssimLibrary

#Create a library called <model>.ssim in the current working directory.
myLib = ssimLibrary(model="st-sim")
session(myLib) #The SycroSim session
filepath(myLib) #Path to the file on disk.
info(myLib) #Model type and other library information.
#Open an existing SyncroSim library in the current working directory.
myLib = ssimLibrary()
myLib = ssimLibrary(name="st-sim",model="st-sim")

#Create a library with name in the current working directory
#myLib2 = ssimLibrary(name="Lib2",model="st-sim")

#Create a library with a name and model in another directory
myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="st-sim")

#create or load a library using a specific session
mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
myLib = ssimLibrary(name="st-sim",cSession=mySession)



showMethods(class="SSimLibrary")
showMethods(class="Session")
