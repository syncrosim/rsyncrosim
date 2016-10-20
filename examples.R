
library(devtools)
devtools::load_all()
#source("R/generics.R");source("R/ssimLibrary.R");source("R/session.R");source("R/command.R")

#Create a library called <model>.ssim in the current working directory.
myLib = ssimLibrary(model="st-sim")
session(myLib) #The SycroSim session
path(myLib) #Path to the file on disk.
info(myLib) #Model type and other library information.

#Open an existing SyncroSim library in the current working directory.
myLib = ssimLibrary()

#Create a library with name in the current working directory
myLib2 = ssimLibrary(name="Lib2",model="st-sim")

#Create a library with a name and model in another directory
myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="st-sim")

#create or load a library using a specific session
mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
myLib = ssimLibrary(name="Lib2",cSession=mySession)

showMethods(class="SSimLibrary")
showMethods(class="Session")
