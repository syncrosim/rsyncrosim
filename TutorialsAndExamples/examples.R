# Readme-Development.txt explains how to install the rsyncrosim package from github
# This script demonstrates how to use the rsyncrosim package.
# devtools::document();devtools::load_all()
# devtools::test()
#library(rsyncrosim)

#################################
# Examples - querying the package
?session # Help for the Session object constructor
?ssimLibrary # Help for the SsimLibrary object constructor.
showMethods(class="SsimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
getMethod("modules","Session") # See code for the filepath method of the Session object.
showMethods("filepath") # See the objects for which filepath is defined.
?filepath # Help for the filepath function
##########################
# Create and query a session
mySsim = session() # Creates a session using the default installation of syncrosim
devSsim = session("C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug",silent=F) # Creates a non-silent session using a particular version (i.e. folder) of syncrosim
showMethods(class="Session",where=loadNamespace("rsyncrosim"))
filepath(mySsim) # Lists the folder location of syncrosim session
version(mySsim) # Lists the version of syncrosim session

modules(mySsim) # Dataframe of the modules installed with this verions of SyncroSim.
models(mySsim) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
# LOW PRIORITY: Platform agnostic paths. For now, ask Linux users to specify the path to SyncroSim.Console.exe

# Add/remove modules
removeModules(mySsim) = "hi"
# NOTE: this works but causes problems when working with a dev version of syncrosim.
#removeModules(mySsim) = "stsim-stock-flow"
#is.element("stsim-stock-flow",modules(mySsim)$name)
#addModule("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg",mySsim)
#addModule(c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg"),mySsim)
#is.element("stsim-stock-flow",modules(mySsim)$name)

###########################
# Give SyncroSim commands - users won't normally need to do this, but advanced users may.
command("help")
command(c("list","help"),session(printCmd=T))
# LOW PRIORITY: Better explain command with help examples: c("list","models")

# LATER: Create own model from scratch in R. Inputs, output and calculations

################################
# Create a new SsimLibrary
# If no primary model and only one model installed, use that.
models(session())
myLibrary = ssimLibrary(name="stsim")

myLibrary = ssimLibrary() # Uses default syncrosim installation and creates a default ssimLibrary called <module name>.ssim in the current R working directory
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim",session=session())
# See ?ssimLibrary for more details and examples.

# With addons
addons(myLibrary,all=T)
addons(myLibrary)
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim", addon=c("stsim-ecological-departure"))
addons(myLibrary)

myLibrary = ssimLibrary() # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error

# Open an existing ST-Sim library on a SyncroSim connection
myLibrary = ssimLibrary(name="C:/Temp/NewLibrary.ssim")

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
update(myLibrary)
info(myLibrary)

# Backup a library (with various options) - skip this for now
#backup(myLibrary)
#restore(myLibrary)
# LOW PRIORITY


###################################
# Projects
# Create a new project
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim")
myProject = project(ssimLibrary=myLibrary, project="My new project name")

# Get a named list of existing projects
myProjects = project(myLibrary,summary=F) # Each element in the list is named by a character version of the project ID

str(myProjects)
names(myProjects)   # vector of the project ids (using base R names function)
myProjects =project(myLibrary) # Returns a data frame containing project names and ids.

# Get an existing project. Assume that name uniquely identifies a single project - give error if not
myProject = myProjects[["1"]]
myProject = project(myLibrary, project="TempProject")

# Get/set the project properties - for now we can only set the name
name(myProject)
name(myProject) = "New project name"

myLibrary = ssimLibrary(myProject) # Returns a SyncroSimLibrary object for the project

# Delete projects
project(myLibrary)
removeProject(myLibrary, project="My new project name") # Returns "saved" or a failure message, or a list of these for each project.
removeProject(myLibrary, project=c(25),force=T)
# QUESTION: consistency with enable/disableAddons? Assignment operators.

#########################
# Scenarios
# devtools::document();devtools::load_all()
# Get a named list of Scenario objects
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary3.ssim")
myProject = project(myLibrary,project="a project")
scenario(myLibrary)
myScenario = scenario(myLibrary,scenario="a scenario") #works because there is <= 1 project in library
scenario(myLibrary)

myScenario = scenario(myProject, scenario="My new scenario name")
scenario(myLibrary)

scenarioId(myScenario)
myScenarios = scenario(myLibrary,summary=F)
names(myScenarios)
scenario(myLibrary)
project(myLibrary)

scenario(myProject)

# Get a list of existing results scenarios for a particular project
myScenarios = scenario(myProject, summary=F,results=TRUE)

# Get an existing scenario by ID
myScenario = myScenarios[["1"]] # By character ID from the list of scenarios - returns a single scenario object
scenario(myLibrary)
myScenario = scenario(myLibrary, scenario=1) # By ID directly from the library - return a single scenario object

# Delete a scenario
scenario(myLibrary)
removeScenario(myLibrary, scenario=c(3),force=T)
scenario(myLibrary)

# Get/set the scenario properties - for now we can only set Summary tab information (i.e. name, author, description and readOnly)
name(myScenario)
scenarioId(myScenario)

name(myScenario) = "New scenario name"
ssimLibrary(myScenario)  # Returns a SyncroSimLibrary object for the scenario
projectId(myScenario)  # Returns the project ID for the scenario

#Get/set scenario properties
readOnly(myScenario)    # Returns TRUE/FALSE
author(myScenario)
description(myScenario)
setProperties(myScenario, author="Colin Daniel",description="A great scenario.")
author(myScenario)

#TO DO
hasResults(myScenario)    # Returns TRUE/FALSE
results(myScenario)     # returns a named vector (by char ID) of the results scenarios associated with this scenario; returns empty vector if no results

# LOW PRIORITY - datafeeds
# TO DO: disable assignment functions for result scenarios?

# devtools::test()

#############################
# Datasheets
# Get datasheet from an SsimLibrary, Projects or Scenarios.
# NOTE CHANGE: can query multiple projects or scenarios - see ?datasheet for details.
# Datasheets are provided in dataframe format
# We return lookup columns as factors, based on the definitions at the time the datasheet is created
# We also return each column in the correct data type.
# devtools::document();devtools::load_all()
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim")
scenario(myLibrary)
myScenario = scenario(myLibrary,scenario=1)

# x is a SyncroSim object (SsimLibrary,Project or Scenario) or name/path of a library on disk.
# scenario and project can be names, ids, or SycnroSim objects - loadDatasheets does not handle multiple projects/scenarios.
myProjectDataframes = datasheet(myLibrary, project=1, summary=F) # A named list of all the project and library datasheets for project id 2.
#myScenarioDataframes = datasheet(myScenario,summary=F) #This takes a long time to run - so don't.
myProjectSheetNames = subset(datasheet("C:/Temp/NewLibrary.ssim", project=1),scope=="project") # A dataframe of datasheet names for project id 1.
myTransitionTypeGroups = myProjectDataframes[["STSim_TransitionTypeGroup"]] # a single dataframe
# DISCUSS: Default datasheet() retrieval (empty=F, stringsAsFactors=T) requires a database query and at least 1 console call
# A database query is also required for each lookup, so the default datasheet() can be slow.
# datasheet(myScenario,summary=F) is very slow because there are a lot of scenario datasheets.
# Setting empty=T eliminates the database query.
# Setting lookupsAsFactors=T eliminates the console call.
# Getting a datasheet for multiple scenarios or projects requires only 1 extra console call.
# Even so datasheet(name=NULL,summary=F) can be very slow, and pull a lot of data - use rarely, with caution.

datasheets(myScenario,scope="scenario")$name
myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition")

myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition",dependsAsFactors=F) # This option returns characters instead of factors

#TO DO
#myScenarioDataframes = datasheet(myProject, summary=F, keepId=T) # This option returns a dataframe with IDs, not factors

# Similarly we can get dataframes of project datasheets
project(myLibrary)
myProjectDataframes = datasheet(myLibrary, project="Project", summary=F)  # same as above

# Get empty template dataframes - lookup columns are expressed as factors with only valid values
emptyDeterministicTransitionDataframe = datasheet(myScenario, name="STSim_DeterministicTransition",empty=T)

# Update the values for project datasheet - see tutorial for examples.
stateClassDefinition = datasheet(myProject, name="STSim_StateLabelX")
stateClassDefinition=addRows(stateClassDefinition,data.frame(Name=c('Coniferous','Deciduous','Mixed')))
#newStateClassDefinitionDataframe = rbind(existingStateClassDefinitionDataframe, c(NA, "Forest", "All", NA, NA, NA))
# NOTE: rbind does not preserve types and factor levels

# Then save the updated project datasheets back to the library
saveDatasheet(myLibrary, stateClassDefinition, project=1, name="STSim_StateLabelX")

# Update the values of an existing scenario datasheet (after the definitions have been added)
scenario(myProject)
myScenario = scenario(myProject, scenario=1)
myDeterminisiticTransitions = datasheet(myScenario, name="STSim_DeterministicTransition",optional=T)
myDeterminisiticTransitions[1:3,"AgeMin"] = c(50, 60, NA)    # change the AgeMin field for 3 rows - character to allow blanks
#NOTE CHANGE: this syntax preserves types and factor levels, and adds new rows if necessary.

datasheet(myScn)
# Then save the updated scenario datasheet back to the library
#saveDatasheet(myLibrary,myDeterminisiticTransitions, scenario=myScenario, name="STSim_DeterministicTransition")
# NOTE: this fails because myDeterministicTransitions is not complete.
# See commandLineTutorial for working example

#################
# Run
# Run a scenario and return the results scenario
# See commandLineTutorial for working examples
#myResultsScenario = run(myScenario)
#myResultsScenario = run(myLibrary,scenario=509)    # run scenario by ID
#myResultsScenarios= run(myScenarios)  # Run a list of scenarios

# Get the output from the results scenario - note that only results scenarios have scenario output datafeeds
#myoutputDataframe = datasheet(myResultsScenario, name="STSim_OutputStratumState")

# Need to figure out how to deal with raster scenario datafeeds (both input and output)...


####################
# Other examples
# Create a library called <model>.ssim in the current working directory.
# devtools::document();devtools::load_all()

myLib = ssimLibrary()
session(myLib) # The SycroSim session
filepath(myLib) # Path to the file on disk.
info(myLib) # Model type and other library information.
# Open an existing SyncroSim library in the current working directory.
myLib = ssimLibrary()
myLib = ssimLibrary(name="stsim")

# Create a library with name in the current working directory
# myLib2 = ssimLibrary(name="Lib2")

# Create a library with a name and model in another directory
myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"))

# Create or load a library using a specific session
#mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
mySession=session()
myLib = ssimLibrary(name="stsim",session=mySession)
