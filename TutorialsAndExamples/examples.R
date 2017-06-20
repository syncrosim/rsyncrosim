# Readme-Development.txt explains how to install the rsyncrosim package from github
# This script demonstrates how to use the rsyncrosim package.
# devtools::document();devtools::load_all()
# devtools::test()
#library(rsyncrosim)

#################################
# Examples - querying the package
?session # Help for the Session object constructor
?ssimLibrary # Help for the function to make or get an SsimLibrary object.
showMethods(class="SsimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
getMethod("modules","Session") # See code for the filepath method of the Session object.
showMethods("filepath") # See the objects for which filepath is defined. SsimLibrary, Project and Scenario objects inherit from SsimObject.
?filepath # Help for the filepath function
##########################
# Create and query a session
mySsim = session() # Creates a session using the default installation of syncrosim
devSsim = session("C:/gitprojects/syncrosim/_deploy_/current",silent=F) # Creates a non-silent session using a particular version (i.e. folder) of syncrosim
# NOTE: Linux users must specify x. The default installation path is only valid on windows.
showMethods(class="Session",where=loadNamespace("rsyncrosim"))
filepath(mySsim) # The folder location of syncrosim session
version(mySsim) # The version of syncrosim session
version() #Version of the default session

mySession = session(defaultModel="carep",printCmd=T,silent=F) #modify default session settings
defaultModel(mySession)
defaultModel(mySession)="stsim" #prints SyncroSim command calls because printCmd=T
defaultModel(mySession)
mySession = session()
silent(mySession)=F
silent(mySession)

# Add/remove modules
modules(mySession)
deleteModule("hi",mySession)
# NOTE: this works but causes problems when working with a dev version of syncrosim.
#deleteModule("sample-basic-dotnet") #using default session
#modules(mySession)
#deleteModule(c("sample-stime-dotnet","sample-spatial-dotnet"),mySession) #a vector, using a particular session
#modules(mySession)

#TO DO: test addModule when we have a real installation of SyncroSim v2
#addModule("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg",mySsim)
#addModule(c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg"),mySsim)
#is.element("stsim-stock-flow",modules(mySsim)$name)

model(mySession) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
modules(mySession) #NOTE: model(mySession) is not a subset of modules(mySession). Change in SyncroSim if necessary.
model()
modules()

###########################
# Give SyncroSim commands - users won't normally need to do this, but advanced users may.

#Three different ways to provide args to command
command(c("create","help"))
command("--create --help",session=session(printCmd=T))
command(list(create=NULL,help=NULL))

args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp.ssim"),model="hello:model-transformer")
output = command(args,session=session(printCmd=T,silent=F))

################################
# Create a new SsimLibrary
myLibrary = ssimLibrary(name="temp") #create new library using default model and default session
model(myLibrary)

myLibrary = ssimLibrary() # Uses default syncrosim installation and creates a default ssimLibrary called SsimLibrary.ssim in the current R working directory
filepath(myLibrary)
myLibrary = ssimLibrary("C:/Temp/NewLibrary.ssim",session=mySession)
filepath(myLibrary)
# See ?ssimLibrary for more details and examples.

# With addons
addons(myLibrary)
addons(myLibrary,all=T)
addons(mySession)
delete(myLibrary,force=T)
#myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim", addon=c("stsim-ecological-departure")) #returns an error because the addon doesn't exist
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim", addon=c("stsim-stockflow")) 
addons(myLibrary)
disableAddon(myLibrary,"stsim-stockflow")
addons(myLibrary)
enableAddon(myLibrary,"stsim-stockflow")
addons(myLibrary)

myLibrary = ssimLibrary() # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error

# Get/set the various properties of the library
session(myLibrary) # returns the SyncroSim Session object associated with the library
session(myLibrary)=session() 

model(myLibrary) # Returns the info about the model of the library. Can't change once library is created.
ssimUpdate(myLibrary)
info(myLibrary)

name(myLibrary)
name(myLibrary)="Fred"
name(myLibrary) 
filepath(myLibrary)#Note that the filename on disk has not changed
backup(myLibrary)
description(myLibrary) = "A new description.\nTry a linebreak." #NOTE: \n adds a linebreak to the description
description(myLibrary) #QUESTION: Each element of the vector is a line of the description. Should this change?
owner(myLibrary) ="Fred"
owner(myLibrary)
readOnly(myLibrary)=T
readOnly(myLibrary)
readOnly(myLibrary)=F
readOnly(myLibrary) 
dateModified(myLibrary)

####################################
#Datasheets, projects, and scenarios 
delete(paste0(getwd(),"/temp26.ssim"),force=T) #delete a library specified by a path
delete(paste0(getwd(),"/temp27.ssim"),force=T)
#project(myLib) #Fails with message that library does not exist on disk.
myLib=ssimLibrary(name="temp26",session=mySession)
delete(myLib,force=T) #delete a library specified by an object
myLib=ssimLibrary(name="temp26",session=mySession)
myOtherLib = ssimLibrary(name="temp27",session=mySession)

myOtherScn = scenario(myOtherLib,scenario="other")
scenario(myOtherLib)
delete(myOtherLib,scenario="other",force=T)
myOtherScn = scenario(myOtherLib,scenario="other2")

project(myOtherLib)
scenario(myOtherLib)

myProject = project(myLib,project="temp")
datasheet(myProject) #Only scope, name and displayName returned
datasheet(myLib,project="temp") #same thing, but more system calls. Generally using ids/objects is faster than using names.
str(datasheet(myProject,optional=T)) #all info
#NOTE: data column only available for scenario scope datasheets
#NOTE: dataInherited and dataSource columns added if there are dependencies. 

project(myLib)

#scenario(myLib,scenario=1) # Fail: need a name to create a scenario
myScn = scenario(myLib,scenario="one") #Ok because only one project in the library.
scenario(myLib)
project(myLib)
myProject = project(myLib,project="temp2")
myScn = scenario(myLib,scenario="one") #Ok because only one scenario of this name occurs in the library.
myScn = scenario(myProject,scenario="one") #Creates a new scenario called "one" in the second project.

#myScn = scenario(myLib,scenario="one") #Fails because now there are two scenarios called "one" in the library.
scenario(myLib)
myScn = scenario(myProject,scenario="one",overwrite=T) #Overwrites existing scenario, assigns new id.
scenario(myLib)
myScn = scenario(myProject,scenario="two",overwrite=T,sourceScenario=1) #Can copy scenarios between projects.
scenario(myLib)
myScn = scenario(myProject,scenario="other",overwrite=T,sourceScenario=myOtherScn) #Can copy scenarios between libraries if sourceScenario is a scenario object.
scenario(myLib)

myOtherProject=project(myOtherLib,project="copy",sourceProject=myProject)#Can copy projects among libraries provided that sourceProject is a Project object.

project(myLib)
myOtherProject=project(myLib,project="copy",sourceProject=10)#Copy a project within the same library.
project(myLib)
myOtherProject=project(myLib,project="temp",sourceProject="temp2")#Warns that sourceProject is ignored because "temp" already exists.
myOtherProject=project(myLib,project="copy2",sourceProject="temp2")#Copy a project by name
project(myLib)

scenario(myLib)
projectId(myProject)
delete(myProject,scenario="one",force=T)
myScn = scenario(myProject,scenario="one",sourceScenario="one") #Ok because only one possible source
myScn = scenario(myProject,scenario="one",sourceScenario="one") #Warns that sourceScenario will be ignored.
#myScn = scenario(myProject,scenario="three",sourceScenario="one") #Fail if more than one scenario named sourceScenario in the library.
scenarioId(myScn)
scenario(myScn,summary=T) #return summary info

aSheet = datasheet(myScn,"SSim_Files")#returns a datasheet
str(aSheet)

aSheet = datasheet(myScn,"SSim_Files",forceElements=T) #returns a list
str(aSheet)

someSheets = datasheet(myScn,c("SSim_Settings","SSim_Files")) #returns a list
str(someSheets)

allScns = scenario(myLib,summary=F)
names(allScns)
someSheets = datasheet(myLib,c("STSim_RunControl","STSim_Transition"),scenario=as.numeric(names(allScns))) #returns a list - each sheet contains scenario info if appropriate
str(someSheets)
someSheets = datasheet(allScns,c("STSim_RunControl","STSim_Transition")) #returns a list - each sheet contains scenario info if appropriate
str(someSheets)

aSheet = datasheet(myScn,"STSim_RunControl",scenario=1)#Warn of conflict between ssimObject and scenario arguments.
aSheet = datasheet(myProject,"STSim_StateClass",project=1)#Warn of conflict between ssimObject and project arguments.
anotherScn = scenario(myProject,"another scn")
aSheet = datasheet(allScns,"STSim_RunControl",scenario=anotherScn)#Warn that project/scenario arguments are ignored when ssimObject is a list of Project/Scenario objects.

myScn = scenario(myProject,scenario="one")
runLog(myScn) #Returns message if the scenario is not a result scenario.

#get/set properties
name(myProject)
name(myProject) = "New project name"
name(myProject)

name(myScn)
name(myScn) = "New scn name"
name(myScn)

description(myProject) = "A new description.\nTry a linebreak." #NOTE: \n adds a linebreak to the description
description(myProject) 
description(myScn) = "Hi"
description(myScn) 

owner(myProject) ="Fred"
owner(myProject)
owner(myScn) ="Alice"
owner(myScn)

readOnly(myProject)=T
readOnly(myProject)
readOnly(myProject)=F
readOnly(myProject) 

readOnly(myScn)=T
readOnly(myScn)

dateModified(myProject)
dateModified(myScn)
parentId(myScn)#NA for scenarios that aren't results.

myLib = ssimLibrary(myProject) #get parent library
mySession = session(myProject) #get parent session
myLib = ssimLibrary(myScn) #get parent library
mySession = session(myScn) #get parent session
myProject=project(myScn) #get parent project

scenarioId(myScn)
projectId(myScn)
projectId(myProject)
filepath(myScn)
filepath(myProject)
ssimUpdate(myScn)
ssimUpdate(myProject)

#############################
# Datasheets
# Get datasheet from an SsimLibrary, Projects or Scenarios.
# Datasheets are provided in dataframe format
# We return lookup columns as factors, based on the definitions at the time the datasheet is created
# We also return each column in the correct data type.
# devtools::document();devtools::load_all()
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim")
scenario(myLibrary)
myScenario = scenario(myLibrary,scenario="one")

myLibraryDataframes = datasheet(myLibrary, summary=F) # A named list of all the library datasheets for project id 2.
#myScenarioDataframes = datasheet(myScenario,summary=F) #This takes a long time to run - so don't.
myProjectSheetNames = subset(datasheet("C:/Temp/NewLibrary.ssim", project=1),scope=="project") # A dataframe of datasheet names for project id 1.
myTransitionTypeGroups = myProjectDataframes[["STSim_TransitionTypeGroup"]] # a single dataframe
# NOTE: datasheet(name=NULL,summary=F) can be very slow, and pull a lot of data - use rarely, with caution.

subset(datasheet(myScenario),scope=="scenario")$name
myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition")
myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition",lookupsAsFactors=F) # This option returns characters instead of factors

#NOTE: option to return IDs rather than values for lookups postponed to a later version. See --export --rawvalues for support from SyncroSim.

# Get empty template dataframes - lookup columns are expressed as factors with only valid values
emptyDeterministicTransitionDataframe = datasheet(myScenario, name="STSim_DeterministicTransition",empty=T)

# Update the values for project datasheet - see tutorial for more examples.
stateClassDefinition = datasheet(myProject, name="STSim_StateLabelX")
stateClassDefinition=addRows(stateClassDefinition,data.frame(Name=c('Coniferous','Deciduous','Mixed')))
# NOTE: rbind does not preserve types and factor levels
saveDatasheet(myProject, stateClassDefinition, name="STSim_StateLabelX")
#RESUME HERE - something is wrong.

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
