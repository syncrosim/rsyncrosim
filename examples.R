# Readme-Development.txt explains how to install the rsyncrosim package from github
# This script demonstrates how to use the rsyncrosim package.
# devtools::document();devtools::load_all()
library(rsyncrosim)

#################################
# Examples - querying the package
?session # Help for the Session object constructor
?ssimLibrary # Help for the SSimLibrary object constructor.
showMethods(class="Session",where=loadNamespace("rsyncrosim")) # See methods for the Session object
getMethod("modules","Session") # See code for the filepath method of the Session object.
showMethods("filepath") # See the objects for which filepath is defined.
?filepath # Help for the filepath function

##########################
# Create and query a session
mySsim = session() # Creates a session using the default installation of syncrosim
mySsim = session("C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug",silent=F) # Creates a silent session using a particular version (i.e. folder) of syncrosim
showMethods(class="Session",where=loadNamespace("rsyncrosim"))
filepath(mySsim) # Lists the folder location of syncrosim session
version(mySsim) # Lists the version of syncrosim session

modules(mySsim) # Dataframe of the modules installed with this verions of SyncroSim.
models(mySsim) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
# TO DO: Get the options for the model argument in library creation.
# LOW PRIORITY: Platform agnostic paths. For now, ask Linux users to specify the path to SyncroSim.Console.exe

# Add/remove modules
removeModules(mySsim) = "dgsim"
removeModules(mySsim) = c("dgsim", "cce-lite")
addModules(mySsim) = "C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
addModules(mySsim) = c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
# How to get the options for adding?
# Understand queue.

###########################
# Give SyncroSim commands - users won't normally need to do this, but advanced users may.
command(list(help=NULL))
command(args=list(list=NULL,help=NULL),mySsim,printCmd=T)
command(list(list=NULL,models=NULL))

# LOW PRIORITY: Accept simpler args, and better explain command with help examples: c("list","models")

# TO DO: handle spaces in paths
# TO DO: How to handle return codes from SyncroSim?
# LATER: Create own model from scratch in R. Inputs, output and calculations

################################
# Create a new SSimLibrary
# If no primary model and only one model installed, use that.
# devtools::document();devtools::load_all()

myLibrary = ssimLibrary(model="st-sim",name="st-sim")

myLibrary = ssimLibrary(model="st-sim") # Uses default syncrosim installation and creates a default ssimLibrary called <module name>.ssim in the current R working directory
myLibrary = ssimLibrary(model="st-sim", name= "C:/Temp/NewLibrary.ssim",aSession=session())
# See ?ssimLibrary for more details and examples.
# TO DO: Sort out module/model/transformer names and concepts. The primary transformer names are not as user friendly as it is shown in here of course.  For example, ST-Sim's is really "stsim:model-transformer".

# Not sure how to reference models and add-ons.
myLibrary = ssimLibrary(model="st-sim", name= "C:/Temp/NewLibrary.ssim", addons=c("st-sim-ecological-departure", "st-sim-stock-flow"))
# TO DO: Need console command for creating a library with add-ons.

# Open an existing ST-Sim library on a SyncroSim connection
myLibrary = ssimLibrary(name="C:/Temp/NewLibrary.ssim", backup=TRUE)
myLibrary = ssimLibrary() # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error

# Get/set the various properties of the library
session(myLibrary) # returns the SyncroSim Session object associated with the library
session(myLibrary)=session() #Does this make sense?

modelName(myLibrary) # Returns the name of the library's model - can't change this once the library is created.
modelVersion(myLibrary)   # Returns the version of the library's model
# TO DO: need Console command to get version of library's model.
addons(myLibrary)   # provides a dataframe of the addons and their various properties
# TO DO: need Console command to get addons of a library.



####################
# Other examples
# Create a library called <model>.ssim in the current working directory.
myLib = ssimLibrary(model="st-sim")
session(myLib) # The SycroSim session
filepath(myLib) # Path to the file on disk.
info(myLib) # Model type and other library information.
# Open an existing SyncroSim library in the current working directory.
myLib = ssimLibrary()
myLib = ssimLibrary(name="st-sim",model="st-sim")

# Create a library with name in the current working directory
# myLib2 = ssimLibrary(name="Lib2",model="st-sim")

# Create a library with a name and model in another directory
myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="st-sim")

# Create or load a library using a specific session
mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
myLib = ssimLibrary(name="st-sim",aSession=mySession)
