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
# devtools::document();devtools::load_all()

# Add/remove modules
#removeModules(mySsim) = "stsim-stock-flow"
#is.element("stsim-stock-flow",modules(mySsim)$name)
#addModules(mySsim) = "C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
#addModules(mySsim) = c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
#is.element("stsim-stock-flow",modules(mySsim)$name)
#NOTE: this works but causes problems because I am working with dev version of SyncroSim

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
# devtools::document();devtools::load_all()
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
myProject = project(myLibrary) #If no name is given, creates a project named "Project".
myProject = project(ssimLibrary=myLibrary, name="My new project name")

# Get a named list of existing projects
myProjects = projects(myLibrary) # Each element in the list is named by a character version of the project ID

myProjects =projects(myLibrary) # Returns a data frame containing project names and ids.
str(myProjects)
names(myProjects)   # vector of the project names (using base R names function)
#TO DO: base R function names returns project id's, not names. I don't recommend overwriting the base function for List objects.

# Get an existing project. Assume that name uniquely identifies a single project - give error if not
myProject = myProjects[["1"]]
myProject = project(myLibrary, name="TempProject")

# Get/set the project properties - for now we can only set the name
name(myProject)
name(myProject) = "New project name"

myLibrary = ssimLibrary(myProject) # Returns a SyncroSimLibrary object for the project

# Delete projects
projects(myLibrary,names=T)
#TO DO: force deletion
deleteProjects(myLibrary, project="My new project name") # Returns a list of "Success!" or a failure messages for each project.
deleteProjects(myLibrary, project=c(25))
#QUESTION: Do we want to be consistent about "project" vs "projects" here?
#QUESTION: consistency with enable/disableAddons? Assignment operators.
#QUESTION: generic delete method?

#########################
# Scenarios
# TO DO: understand results scenarios
# devtools::document();devtools::load_all()
# Get a named list of Scenario objects
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
myProject = project(myLibrary) #If no name is given, creates a project named "Project".
name(myProject)
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary)
#QUESTION: In what cases do we want this to work?
#At present a project is required to create a scenario
#Ideas: if no project, create project/scenario?
#Fail if more than one project.
myScenario = scenario(myProject) #Creates if no scenarios exist. Opens if 1 scenario exists. Otherwise complains.
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary,project="My new project name") #Will create project if necessary
scenarios(myLibrary,names=T)
#QUESTION: Default names for new projects and scenarios???

myScenario = scenario(myProject, name="My new scenario name")
scenarios(myLibrary,names=T)

#TO DO
myScenario = scenario(myLibrary, name="Another scenario", author="Colin", description="My description", readOnly=FALSE)
#NOTE: Returns and error if "Another scenario" already exists, but has different properties?

myScenarios = scenarios(myLibrary)
names(myScenarios)
scenarios(myLibrary,names=T)
projects(myLibrary,names=T)

# RESUME HERE
# Get a list of existing results scenarios for a particular project
myScenarios = scenarios(myProject, results=TRUE)
myScenarios = scenarios("C:/Temp/NewLibrary.ssim", project="My new project name", results=TRUE)
myScenarios = scenarios("C:/Temp/NewLibrary.ssim", project=1, results=TRUE)
#NOTE CHANGE: scenarios() is a generic method defined for Project, SSimLibrary, and character object. If given a character string, queries an SSimLibrary of that name.

# Get an existing scenario by ID
myScenario = myScenarios[["1"]] # By character ID from the list of scenarios - returns a single scenario object
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary, id=1) # By ID directly from the library - return a single scenario object
#NOTE: To be consistent with project() I have used name/id in scenario().

# Delete a scenario
scenarios(myLibrary,names=T)
deleteScenarios(myLibrary, scenario=c(3))
scenarios(myLibrary,names=T)

# Get/set the scenario properties - for now we can only set Summary tab information (i.e. name, author, description and readOnly)
name(myScenario)
id(myScenario)
name(myScenario) = "New scenario name"
ssimLibrary(myScenario)  # Returns a SyncroSimLibrary object for the scenario
projectId(myScenario)  # Returns the project ID for the scenario

#TO DO
readOnly(myScenario)    # Returns TRUE/FALSE

#TO DO
hasResults(myScenario)    # Returns TRUE/FALSE
results(myScenario)     # returns a named vector (by char ID) of the results scenarios associated with this scenario; returns empty vector if no results

# LOW PRIORITY - datafeeds

#############################
# Datasheets
# Get a datasheet from an SSimLibrary, Project or Scenario
# Datasheets are provided in dataframe format
# We return lookup columns as factors, based on the definitions at the time the datasheet is created
# We also return each column in the correct data type. This will require replacing blanks from the db with NA values
# devtools::document();devtools::load_all()
myLibrary = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim")
scenarios(myLibrary,names=T)
myScenario = scenario(myLibrary,id=1)

# datasheet() and datasheets() accept any combination of x, project and scenario arguments.
# x is a SyncroSim object (SSimLibrary,Project or Scenario) or name/path of a library on disk.
# scenario and project can be names, ids, or SycnroSim objects
datasheets(myScenario)
# DISCUSS: Returns a dataframe of names by default - there are a lot of datasheets. Usually not necessary to parse them all.

projects(myLibrary,names=T)
myProjectDataframes = datasheets(myLibrary, project=1, names=T) # A named list of all the project and library datasheets for project id 2.
myProjectDataframes = datasheets("C:/Temp/NewLibrary.ssim", project=1, names=T,scope="project") # A named list of all the project datasheets for project id 2
#TO DO: names=F

#RESUME HERE
#TO DO: handle dependencies among datasheets
myScenarioDataframes = datasheets(myProject, scope="project", names=F, stringAsFactors=F) # This option returns characters instead of factors

#TO DO - not clear to me what this one is about
myScenarioDataframes = datasheets(myProject, scope="scenario", keepId=T) # This option returns a dataframe with IDs, not factors

myDeterministicTransitionDataframe = myScenarioDataframes[["STSim_DeterministicTransition"]] # a single dataframe
myDeterministicTransitionDataframe = datasheets(myScenario)["STSim_DeterministicTransition"]
myDeterministicTransitionDataframe = datasheets(ssimLibrary=mySsimLibrary, scenario=509)["STSim_DeterministicTransition"]

?datasheet
datasheet(myLibrary,name="SSim_Settings")

# Alternatively a datasheet can be provided as a Datasheet object  - skip this for now
myScenarioDatasheets = datasheets(scenario=myScenario, dataframe=FALSE, empty=FALSE)    # named vector of datasheet objects, instead of default dataframes

# Similarly we can get dataframes of project definitions
myProjectDataframes = definitions(myProject)   # same as above - just an alternative function that matches UI terminology
myProjectDataframes = datasheets(ssimLibrary=mySsimLibrary, project="My Existing Project", scope="project")  # same as above

# Get empty template dataframes for each scenario datafeed in a project - lookup columns are expressed as factors with only valid values
emptytScenarioDataframes = datasheets(project=myProject, scope="scenario")     # returns empty versions of all the scenario datafeeds for this project
emptytScenarioDataframes = datasheets(ssimLibrary=mySsimLibrary, project=2, scope="scenario")     # same as above
emptyDeterministicTransitionDataframe = datasheet(project=myProject, name="STSim_DeterministicTransition")

# Similarly also get empty dataframes for project definitions
emptyProjectDataframes = datasheets(myProject, scope="project")
emptyProjectDataframes = definitions(myProject)    # same as above

# Update the values for project definitions
existingStateClassDefinitionDataframe = datasheet(project=myProject, name="STSim_StateClass")
newStateClassDefinitionDataframe = rbind(existingStateClassDefinitionDataframe, c(NA, "Forest", "All", NA, NA, NA))

# Then save the updated project definitions back to the library  - committed to db immediately
result = loadDatasheets(newStateClassDefinitionDataframe, ssimLibrary=mySsimLibrary, project=2, datasheets="STSim_StateClass")
# DISCUSS: was written in plural, but need name/frame to be linked in that case.

# Update the values of an existing scenario datasheet (after the definitions have been added)
myScenario = scenario(project=myProject, scenario=509)
myDeterminisiticTransitionDataframe = datasheets(scenario=myScenario, name="STSim_DeterministicTransition")
myDeterminisiticTransitionDataframe$AgeMin = c(50, 60, NA)    # change the AgeMin field for 3 rows - character to allow blanks

# Then save the updated scenario datasheet back to the library  - committed to db immediately
result = loadDatasheets(myDeterminisiticTransitionDataframe, library=mySsimLibrary, scenario=myScenario, datasheets="STSim_DeterministicTransition")


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
