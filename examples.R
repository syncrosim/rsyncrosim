# Readme-Development.txt explains how to install the rsyncrosim package from github
# This script demonstrates how to use the rsyncrosim package.
# devtools::document();devtools::load_all()
library(rsyncrosim)

#################################
# Examples - querying the package
?session # Help for the Session object constructor
?ssimLibrary # Help for the SSimLibrary object constructor.
showMethods(class="SSimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
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
# NOTE: this works but causes problems because I am working with dev version of SyncroSim
#removeModules(mySsim) = "stsim-stock-flow"
#is.element("stsim-stock-flow",modules(mySsim)$name)
#addModules(mySsim) = "C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
#addModules(mySsim) = c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
#is.element("stsim-stock-flow",modules(mySsim)$name)

###########################
# Give SyncroSim commands - users won't normally need to do this, but advanced users may.
command("help")
command(c("list","help"),mySsim,printCmd=T)
# LOW PRIORITY: Better explain command with help examples: c("list","models")

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
update(myLibrary)

# Backup a library (with various options) - skip this for now
#backup(myLibrary)
#restore(myLibrary)
# LOW PRIORITY

###################################
# Projects
# Create a new project
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
myProject = project(myLibrary) #If no name is given, creates a project named "Project".
myProject = project(ssimLibrary=myLibrary, name="My new project name")

# Get a named list of existing projects
myProjects = projects(myLibrary) # Each element in the list is named by a character version of the project ID

myProjects =projects(myLibrary) # Returns a data frame containing project names and ids.
str(myProjects)
names(myProjects)   # vector of the project names (using base R names function)
# DISCUSS: base R function names returns project id's, not names. I don't recommend overwriting the base function for List objects.

# Get an existing project. Assume that name uniquely identifies a single project - give error if not
myProject = myProjects[["1"]]
myProject = project(myLibrary, name="TempProject")

# Get/set the project properties - for now we can only set the name
name(myProject)
name(myProject) = "New project name"

myLibrary = ssimLibrary(myProject) # Returns a SyncroSimLibrary object for the project

# Delete projects
projects(myLibrary,names=T)
deleteProjects(myLibrary, project="My new project name") # Returns a list of "Success!" or a failure messages for each project.
deleteProjects(myLibrary, project=c(25),force=T)
# QUESTION: Do we want to be consistent about "project" vs "projects" here?
# QUESTION: consistency with enable/disableAddons? Assignment operators.
# QUESTION: generic delete method?

#########################
# Scenarios
# devtools::document();devtools::load_all()
# Get a named list of Scenario objects
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary3.ssim")
myProject = project(myLibrary) #If no name is given, creates a project named "Project".
name(myProject)
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary)
# NOTE: this works only if there is <= 1 project in Library.
myScenario = scenario(myProject) #Creates if no scenarios exist. Opens if 1 scenario exists. Otherwise complains.
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary,project="My new project name") #Will create project if necessary
scenarios(myLibrary,names=T)
# QUESTION: Default names for new projects and scenarios???

myScenario = scenario(myProject, name="My new scenario name")
scenarios(myLibrary,names=T)

myScenario = scenario(myProject, name="Another scenario", author="Colin", description="My description", readOnly=FALSE)
id(myScenario)
# NOTE: Returns and error if "Another scenario" already exists, but has different properties?
myScenarios = scenarios(myLibrary)
names(myScenarios)
scenarios(myLibrary,names=T)
projects(myLibrary,names=T)

scenarios(myProject,names=T)

# Get a list of existing results scenarios for a particular project
myScenarios = scenarios(myProject, results=TRUE)
myScenarios = scenarios("C:/Temp/NewLibrary.ssim", project="My new project name", results=TRUE)
myScenarios = scenarios("C:/Temp/NewLibrary.ssim", project=1, results=TRUE)
# NOTE CHANGE: scenarios() is a generic method defined for Project, SSimLibrary, and character object. If given a character string, queries an SSimLibrary of that name.

# Get an existing scenario by ID
myScenario = myScenarios[["1"]] # By character ID from the list of scenarios - returns a single scenario object
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary, id=1) # By ID directly from the library - return a single scenario object
# NOTE: To be consistent with project() I have used name/id in scenario().

# Delete a scenario
scenarios(myLibrary,names=T)
deleteScenarios(myLibrary, scenario=c(3),force=T)
scenarios(myLibrary,names=T)

# Get/set the scenario properties - for now we can only set Summary tab information (i.e. name, author, description and readOnly)
name(myScenario)
id(myScenario)
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

#############################
# Datasheets
# Get datasheet from an SSimLibrary, Projects or Scenarios.
# NOTE CHANGE: can query multiple projects or scenarios - see ?datasheet for details.
# Datasheets are provided in dataframe format
# We return lookup columns as factors, based on the definitions at the time the datasheet is created
# We also return each column in the correct data type.
# devtools::document();devtools::load_all()
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary,id=1)

# NOTE: datasheet(), datasheets() and loadDatasheets() accept any combination of x, project and scenario arguments.
# x is a SyncroSim object (SSimLibrary,Project or Scenario) or name/path of a library on disk.
# scenario and project can be names, ids, or SycnroSim objects - loadDatasheets does not handle multiple projects/scenarios.
myProjectDataframes = datasheets(myLibrary, project=1, names=F) # A named list of all the project and library datasheets for project id 2.
#myScenarioDataframes = datasheets(myScenario,names=F) #This takes a long time to run - so don't.
myProjectSheetNames = datasheets("C:/Temp/NewLibrary.ssim", project=1,scope="project") # A dataframe of datasheet names for project id 1.
myTransitionTypeGroups = myProjectDataframes[["STSim_TransitionTypeGroup"]] # a single dataframe
#myDeterministicTransitionDataframe = datasheets(myScenario)["STSim_DeterministicTransition"]
#myDeterministicTransitionDataframe = datasheets(ssimLibrary=mySsimLibrary, scenario=509)["STSim_DeterministicTransition"]
# DISCUSS: Default datasheet() retrieval (empty=F, stringsAsFactors=T) requires a database query and at least 1 console call
# A database query is also required for each lookup, so the default datasheet() can be slow.
# datasheets(myScenario,names=F) is very slow because there are a lot of scenario datasheets.
# Setting empty=T eliminates the database query.
# Setting lookupsAsFactors=T eliminates the console call.
# Getting a datasheet for multiple scenarios or projects requires only 1 extra console call.
# Even so datasheets() slow.

datasheets(myScenario,scope="scenario")$name
myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition")

myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition",dependsAsFactors=F) # This option returns characters instead of factors
#myScenarioDataframes = datasheets(myProject, scope="project", names=F, stringAsFactors=F)

#TO DO
#myScenarioDataframes = datasheets(myProject, scope="scenario", keepId=T) # This option returns a dataframe with IDs, not factors

# Alternatively a datasheet can be provided as a Datasheet object  - skip this for now
#myScenarioDatasheets = datasheets(scenario=myScenario, dataframe=FALSE, empty=FALSE)    # named vector of datasheet objects, instead of default dataframes
#DISCUSS - not sure exactly what a datasheet object should be, or why we need one.

# Similarly we can get dataframes of project definitions
myProjectDataframes = definitions(myProject)   # same as above - just an alternative function that matches UI terminology
projects(myLibrary,names=T)
myProjectDataframes = datasheets(myLibrary, project="Project", scope="project")  # same as above

# Get empty template dataframes for each scenario datafeed in a project - lookup columns are expressed as factors with only valid values
# NOTE: slow. don't do it.
#emptyScenarioDataframes = datasheets(myProject, scope="scenario",empty=T,names=F)     # returns empty versions of all the scenario datafeeds for this project
#emptyScenarioDataframes = datasheets(myLibrary, project=1, scope="scenario",empty=T,names=F)     # same as above
emptyDeterministicTransitionDataframe = datasheet(myScenario, name="STSim_DeterministicTransition",empty=T)

# Similarly also get empty dataframes for project definitions
#emptyProjectDataframes = datasheets(myProject, scope="project")
#emptyProjectDataframes = definitions(myProject)    # same as above

# Update the values for project definitions - see tutorial for examples.
stateClassDefinition = datasheet(myProject, name="STSim_StateLabelX")
addRows(stateClassDefinition) = data.frame(Name=c('Coniferous','Deciduous','Mixed'))
#newStateClassDefinitionDataframe = rbind(existingStateClassDefinitionDataframe, c(NA, "Forest", "All", NA, NA, NA))
# NOTE: rbind does not preserve types and factor levels
# NOTE CHANGE: addRows does in place modification of dataframe. This requires less typing. Can be changed.

# Then save the updated project definitions back to the library
loadDatasheets(myLibrary, stateClassDefinition, project=1, name="STSim_StateLabelX")
# DISCUSS: This was written in plural, but need name/frame to be linked in that case.

# Update the values of an existing scenario datasheet (after the definitions have been added)
scenarios(myProject,names=T)
myScenario = scenario(myProject, id=1)
myDeterminisiticTransitions = datasheet(myScenario, name="STSim_DeterministicTransition",optional=T)
myDeterminisiticTransitions[1:3,"AgeMin"] = c(50, 60, NA)    # change the AgeMin field for 3 rows - character to allow blanks
#NOTE CHANGE: this syntax preserves types and factor levels, and adds new rows if necessary.

# Then save the updated scenario datasheet back to the library
#loadDatasheets(myLibrary,myDeterminisiticTransitions, scenario=myScenario, name="STSim_DeterministicTransition")
# NOTE: this fails because myDeterministicTransitions is not complete.
# See commandLineTutorial for working example

#################
# Run
# Run a scenario and return the results scenario  - note that it is up to user to save changes to db before running
# See commandLineTutorial for working examples
#myResultsScenario = run(myScenario)
#myResultsScenario = run(scenario=509)    # run scenario by ID
#myResultsScenarios= run(myScenarios)  # Run a vector of scenarios (or a vector of IDs)  - skip this for now


# Get the output from the results scenario - note that only results scenarios have scenario output datafeeds
#myoutputDataframe = datasheets(scenario=myResultsScenario, name="STSim_OutputStratumState")

# Need to figure out how to deal with raster scenario datafeeds (both input and output)...


####################
# Other examples
# Create a library called <model>.ssim in the current working directory.
# devtools::document();devtools::load_all()

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
#mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
mySession=session()
myLib = ssimLibrary(name="stsim",session=mySession)
