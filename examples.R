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
devSsim = session("C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug",silent=F) # Creates a silent session using a particular version (i.e. folder) of syncrosim
showMethods(class="Session",where=loadNamespace("rsyncrosim"))
filepath(mySsim) # Lists the folder location of syncrosim session
version(mySsim) # Lists the version of syncrosim session

modules(mySsim) # Dataframe of the modules installed with this verions of SyncroSim.
models(mySsim) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
# LOW PRIORITY: Platform agnostic paths. For now, ask Linux users to specify the path to SyncroSim.Console.exe

# Add/remove modules
#removeModules(mySsim) = "stsim-stock-flow"
#is.element("stsim-stock-flow",modules(mySsim)$name)
#addModules(mySsim) = "C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
#addModules(mySsim) = c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
#is.element("stsim-stock-flow",modules(mySsim)$name)
#NOTE: this works, but causes problems because I am using the dev version of SyncroSim.

###########################
# Give SyncroSim commands - users won't normally need to do this, but advanced users may.
command(list(help=NULL))
command(args=list(list=NULL,help=NULL),mySsim,printCmd=T)
command(list(list=NULL,models=NULL))

# LOW PRIORITY: Accept simpler args, and better explain command with help examples: c("list","models")
# LATER: Create own model from scratch in R. Inputs, output and calculations

################################
# Create a new SSimLibrary
# If no primary model and only one model installed, use that.
models(session())
myLibrary = ssimLibrary(model="stsim",name="stsim")

myLibrary = ssimLibrary(model="stsim") # Uses default syncrosim installation and creates a default ssimLibrary called <module name>.ssim in the current R working directory
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=session())
# See ?ssimLibrary for more details and examples.

# With addons
addons(myLibrary,all=T)
addons(myLibrary)
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim", addons=c("stsim-ecological-departure"))
addons(myLibrary)
#TO DO: apply updates to a library from the command line?

myLibrary = ssimLibrary() # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error

# Open an existing ST-Sim library on a SyncroSim connection
myLibrary = ssimLibrary(model="stsim",name="C:/Temp/NewLibrary.ssim", backup=TRUE)

# Get/set the various properties of the library
session(myLibrary) # returns the SyncroSim Session object associated with the library
session(myLibrary)=session() #Does this make sense?

modelName(myLibrary) # Returns the name of the library's model - can't change this once the library is created.
modelVersion(myLibrary)   # Returns the version of the library's model
addons(myLibrary,all=T)   # A dataframe of enabled addons. Set all=T to see disabled addons.
enableAddons(myLibrary) = c("stsim-stock-flow")
addons(myLibrary)
disableAddons(myLibrary) = c("stsim-ecological-departure", "stsim-stock-flow")
addons(myLibrary)

# Backup a library (with various options) - skip this for now
#backup(myLibrary)
#restore(myLibrary)
# LOW PRIORITY

###################################
# Projects
# Create a new project
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
myProject = project(myLibrary) #If no name is given, creates a project named "Project<ID>".
myProject = project(ssimLibrary=myLibrary, name="My new project name")

# Get a named list of existing projects
myProjects = projects(myLibrary) # Each element in the list is named by a character version of the project ID

projects(myLibrary,names=T) # Returns a data frame containing project names and ids.
names(myProjects)   # vector of the project names (using base R names function)
#TO DO: base R function names returns project id's, not names. I don't think it is a good idea to overwrite the base function for List objects.

# Get an existing project. Assume that name uniquely identifies a single project - give error if not
myProject = myProjects[["1"]]
myProject = project(myLibrary, name="TempProject")

# Get/set the project properties - for now we can only set the name
name(myProject)
name(myProject) = "New project name"
#TO DO: get console command for renaming a project

myLibrary = ssimLibrary(myProject) # Returns a SyncroSimLibrary object for the project

# Delete projects
#RESUME HERE
projects(myLibrary,names=T)
deleteProjects(myLibrary, project="My new project name") # Returns a list of "Success!" or a failure messages for each project.
deleteProjects(myLibrary, project=c(37,61))
#QUESTION: Do we want to be consistent about "project" vs "projects" here?
#QUESTION: consistency with enable/disableAddons?

#########################
# Scenarios
# TO DO: understand results scenarios
# Get a named list of Scenario objects
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
myProject = project(myLibrary) #If no name is given, creates a project named "Project<ID>".
myScenario = scenario(myLibrary)
#QUESTION: In what cases do we want this to work?
#For now a project is required to create a scenario
myScenario = scenario(myProject) #Creates or loads a scenario named Scenario
myScenario = scenario(myLibrary,project="My new project name")
#QUESTION: Default names for new projects and scenarios???

myScenario = scenario(myProject, name="My new scenario name")

#TO DO
myScenario = scenario(myLibrary, name="Another scenario", author="Colin", description="My description", readOnly=FALSE)
#NOTE: Returns and error if "Another scenario" already exists, but has different properties?

myScenarios = scenarios(myLibrary)
names(myScenarios)
scenarios(myLibrary,names=T)
projects(myLibrary,names=T)

# Get a list of existing results scenarios for a particular project
myScenarios = scenarios(myProject, results=TRUE)
myScenarios = scenarios("C:/Temp/NewLibrary.ssim", project="My new project name", results=TRUE)
myScenarios = scenarios("C:/Temp/NewLibrary.ssim", project=2, results=TRUE)
#NOTE CHANGE: scenarios() is a generic method defined for Project, SSimLibrary, and character object. If given a character string, queries an SSimLibrary of that name.

# Get an existing scenario by ID
myScenario = myScenarios[["1"]] # By character ID from the list of scenarios - returns a single scenario object
myScenario = scenario(myLibrary, id=2) # By ID directly from the library - return a single scenario object
#NOTE: To be consistent with project() I have used name/id in scenario().

# Delete a scenario
scenarios(myProject,names=T)
deleteScenarios(myProject, scenario=c(1,3))

# devtools::document();devtools::load_all()
# RESUME HERE
# Get/set the scenario properties - for now we can only set Summary tab information (i.e. name, author, description and readOnly)
name(myScenario)
name(myScenario) = "New scenario name"
id(myScenario)
readOnly(myScenario)    # Returns TRUE/FALSE

hasResults(myScenario)    # Returns TRUE/FALSE
projectId(myScenario)  # Returns the project ID for the scenario
ssimLibrary(myScenario)  # Returns a SyncroSimLibrary object for the scenario
results(myScenario)     # returns a named vector (by char ID) of the results scenarios associated with this scenario; returns empty vector if no results


####################
# Other examples
# Create a library called <model>.ssim in the current working directory.
myLib = ssimLibrary(model="stsim")
session(myLib) # The SycroSim session
filepath(myLib) # Path to the file on disk.
info(myLib) # Model type and other library information.
# Open an existing SyncroSim library in the current working directory.
myLib = ssimLibrary()
myLib = ssimLibrary(name="stsim",model="stsim")

# Create a library with name in the current working directory
# myLib2 = ssimLibrary(name="Lib2",model="stsim")

# Create a library with a name and model in another directory
myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="stsim")

# Create or load a library using a specific session
mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
myLib = ssimLibrary(name="stsim",session=mySession)
