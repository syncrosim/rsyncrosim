#Readme-Development.txt explains how to install the rsyncrosim package from github
#This script demonstrates how to use the rsyncrosim package.
#devtools::document();devtools::load_all()
library(rsyncrosim)

#################################
#Examples - querying the package
?session #Help for the Session object constructor
?ssimLibrary #Help for the SSimLibrary object constructor.
showMethods(class="Session",where=loadNamespace("rsyncrosim")) #See methods for the Session object
getMethod("filepath","Session") #See code for the filepath method of the Session object.
showMethods("filepath") #See the objects for which filepath is defined.
?filepath #Help for the filepath function

##########################
#Create and query a session
mySsim = session()   # Creates a session using the default installation of syncrosim
mySsim = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe",silent=T)   # Creates a silent session using a particular version (i.e. folder) of syncrosim
showMethods(class="Session",where=loadNamespace("rsyncrosim"))
filepath(mySsim)   # Lists the folder location of syncrosim session
version(mySsim)   # Lists the version of syncrosim session
modules(mySsim)   # Dataframe of the modules installed with this version of syncrosim, listing all of its properties as columns
#JH Not sure how modules differ from models. Currently returns a dataframe of models returned by --list --models.
#TO DO: Get the options for the model argument in library creation.
#TO DO: figure out how to handle the output when more than one module is available.

# Add/remove modules
addModules(mySsim) = "C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
addModules(mySsim) = c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
removeModules(mySsim) = "stsim"
removeModules(mySsim) = c("stsim", "stsim-stock-flow")
#TO DO: addModules and removeModules don't yet do anything. Need console commands for adding and removing modules

###########################
#Give SyncroSim commands - users won't normally need to do this, but advanced users may.
command(args=list(help=NULL),mySsim)
command(args=list(list=NULL,help=NULL),mySsim)
command(args=list(list=NULL,models=NULL),mySsim)

################################
# Create a new SSimLibrary
mySsimLibrary = ssimLibrary(model="st-sim")     # Uses default syncrosim installation and creates a default ssimLibrary called <module name>.ssim in the current R working directory
mySsimLibrary = ssimLibrary(model="st-sim", name= "C:/Temp/NewLibrary.ssim",cSession=session())
#see ?ssimLibrary for more details and examples.

# Not sure how to reference models and add-ons. They are different from modules, as a module (like an R package) can contain anything, including
# multiple models and add-ons. Will need to work with Leo and Alex on this.
mySsimLibrary = ssimLibrary(model="st-sim", name= "C:/Temp/NewLibrary.ssim", addons=c("st-sim-ecological-departure", "st-sim-stock-flow"))
#TO DO: Need console command for creating a library with add-ons.

# Open an existing ST-Sim library on a SyncroSim connection
mySsimLibrary = ssimLibrary(name="C:/Temp/NewLibrary.ssim", backup=TRUE)
mySsimLibrary = ssimLibrary()      # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error

# Get/set the various properties of the library
session(mySsimLibrary)   # returns the SyncroSim Session object associated with the library
session(mySsimLibrary)=session()
modelName(mySsimLibrary)    # returns the name of the library's model - can't change this once the library is created.
modelVersion(mySsimLibrary)   # returns the version of the library's model
#TO DO: need Console command to get version of library's model.
addons(mySsimLibrary)   # provides a dataframe of the addons and their various properties
#TO DO: need Console command to get addons of a library.

####################
#Other examples
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
