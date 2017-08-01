# Readme-Development.txt explains how to install the rsyncrosim package from github
# This script demonstrates how to use the rsyncrosim package.
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
#library(rsyncrosim)

#################################
# Examples - querying the package
?session # Help for the Session object constructor
?ssimLibrary # Help for the function to make or get an SsimLibrary object.
showMethods(class="SsimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
getMethod("module","Session") # See code for the module method of the Session object.
showMethods("filepath") # See the objects for which filepath is defined. SsimLibrary, Project and Scenario objects inherit from SsimObject.
?filepath # Help for the filepath function
##########################
# Create and query a session
sessionPath = "c:/gitprojects/syncrosim/_deploy_/current"
mySsim = session() # Creates a session using the default installation of syncrosim.
# NOTE: Linux users must specify x. The default installation path is only valid on windows.
mySsim = session(sessionPath,silent=F) # Creates a non-silent session using a particular version (i.e. folder) of syncrosim
showMethods(class="Session",where=loadNamespace("rsyncrosim"))
filepath(mySsim) # The folder location of syncrosim session
version(mySsim) # The version of syncrosim session
version() #Version of the default session.

mySession = session(sessionPath,defaultModel="carep",printCmd=T,silent=F) #modify default session settings
defaultModel(mySession)
defaultModel(mySession)="stsim" #prints SyncroSim command calls because printCmd=T
defaultModel(mySession)
mySession = session(sessionPath)
silent(mySession)=F
silent(mySession)
printCmd(mySession)

# Add/remove modules
module(mySession)
deleteModule("hi",mySession)
# NOTE: this works but causes problems when working with a dev version of syncrosim.
#deleteModule("sample-basic-dotnet") #using default session
#module(mySession)
#deleteModule(c("sample-stime-dotnet","sample-spatial-dotnet"),mySession) #a vector, using a particular session
#module(mySession)

#TO DO: test addModule when we have a real installation of SyncroSim v2
#addModule("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg",mySsim)
#addModule(c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg"),mySsim)
#is.element("stsim-stock-flow",module(mySsim)$name)

model(mySession) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
module(mySession) #NOTE: model(mySession) is not a subset of module(mySession). Change in SyncroSim if necessary.
model() # Dataframe of the models installed with the default version of syncrosim, listing all of its properties as columns 
module() # Dataframe of the modules installed with the default version of syncrosim, listing all of its properties as columns 

###########################
# Give SyncroSim commands - users won't normally need to do this, but advanced users may.

#Three different ways to provide args to command
command(c("create","help"))
command("--create --help",session=session(sessionPath,printCmd=T))
command(list(create=NULL,help=NULL))

delete(paste0(getwd(),"/temp.ssim"),force=T)
args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp.ssim"),model="hello:model-transformer")
output = command(args,session=session(sessionPath,printCmd=T,silent=F))

################################
# Create a new SsimLibrary
myLibrary = ssimLibrary(name="temp",session=mySession) #create new library using default model
model(myLibrary)

myLibrary = ssimLibrary(session=mySession) # creates a default ssimLibrary called SsimLibrary.ssim in the current R working directory
filepath(myLibrary)
myLibrary = ssimLibrary("C:/Temp/NewLibrary.ssim",session=mySession)
filepath(myLibrary)
# See ?ssimLibrary for more details and examples.

# With addons
addon(myLibrary)
addon(mySession)
delete(myLibrary,force=T)

#myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim", addon=c("stsim-ecological-departure")) #returns an error because the addon doesn't exist
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim", addon=c("stsim-stockflow"),session=mySession) 
addon(myLibrary)
disableAddon(myLibrary,"stsim-stockflow")
addon(myLibrary)
enableAddon(myLibrary,"stsim-stockflow")
addon(myLibrary)

myLibrary = ssimLibrary(session=mySession) # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error

# Get/set the various properties of the library
session(myLibrary) # returns the SyncroSim Session object associated with the library
session(myLibrary)=session(sessionPath) #Note new session must have same filepath as old session to avoid SyncroSim version conflicts.

model(myLibrary) # Returns the info about the model of the library. Can't change once library is created.
ssimUpdate(myLibrary)
ssimLibrary(myLibrary) #If ssimObject is SsimLibrary, returns summary info by default
class(ssimLibrary(myLibrary,summary=F)) #Returns the object instead.

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
myLib=ssimLibrary(name="temp26",session=mySession)
delete(myLib,force=T) #delete a library specified by an object
myLib=ssimLibrary(name="temp26",session=mySession)
myOtherLib = ssimLibrary(name="temp27",session=mySession)

myOtherScn = scenario(myOtherLib,scenario="other")
scenario(myOtherLib)
delete(myOtherLib,scenario="other",force=T)
scenario(myOtherLib)
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
myOtherProject=project(myLib,project="copy",sourceProject=1)#Copy a project within the same library.
project(myLib)
myOtherProject=project(myLib,project="temp",sourceProject="temp2")#Warns that sourceProject is ignored because "temp" already exists.
myOtherProject=project(myLib,project="copy2",sourceProject="temp2")#Copy a project by name
anotherCopy=project(myLib,sourceProject=project(myLib,project="copy2",summary=F)) #Copy a project object, give the new project a default name
project(myLib)

scenario(myLib)
projectId(myProject)
delete(myProject,scenario="one",force=T)
myScn = scenario(myProject,scenario="one",sourceScenario="one") #Ok because only one possible source
myScn = scenario(myProject,scenario="one",sourceScenario="one") #Warns that sourceScenario will be ignored.
#myScn = scenario(myProject,scenario="three",sourceScenario="one") #Fail if more than one scenario named sourceScenario in the library.
anotherScn = scenario(myProject,sourceScenario=scenario(myProject,"two")) #copy a scenario object with default name

scenario(myLib)
scenarioId(myScn)
scenario(myScn,summary=T) #return summary info

aSheet = datasheet(myScn,"SSim_Files")#returns a datasheet
str(aSheet)

aSheet = datasheet(myScn,"SSim_Files",forceElements=T) #returns a list
str(aSheet)

someSheets = datasheet(myScn,c("SSim_Settings","SSim_Files")) #returns a list
str(someSheets)

allScns = scenario(myLib,summary=F)
names(allScns) #TODO: This function seems to be returning scenario ID values, not names.
someSheets = datasheet(myLib,c("STSim_RunControl","STSim_OutputOptions"),scenario=as.numeric(names(allScns))) #returns a list - each sheet contains scenario info if appropriate
str(someSheets)
someSheets = datasheet(allScns,c("STSim_RunControl","STSim_OutputOptions")) #returns a list - each sheet contains scenario info if appropriate
str(someSheets)

aSheet = datasheet(myScn,"STSim_RunControl",scenario=1)#Warn of conflict between ssimObject and scenario arguments.
aSheet = datasheet(myProject,"STime_Chart",project=1)#Warn of conflict between ssimObject and project arguments.
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

#test dependency, precedence setting
scenario(myProject)
targetScn = scenario(myProject,scenario="two")
dependency(targetScn)
dependency(targetScn,dependency=c("other","New scn name","another scn")) #elements of the dependency argument are ordered from lowest to highest precedence
dependency(targetScn) #"another scn" was added last, so has highest precedence
dependency(targetScn,dependency=c("another scn","New scn name")) #change the precedence of dependencies by adding them again.
dependency(targetScn) #now "New scn name" has highest precedence.

#print/show methods for SsimObjects and Sessions
myScenario
myProject
myLibrary
mySession


#test delete - vectors of project/scenario/datasheet
scenario(myLib)
datasheet(myProject)
delete(myLib, project=c(1,10),datasheet=c("STime_Chart","STime_DistributionType"),force=T)

delete(myLib, scenario=c(6,7),force=T)
scenario(myLib)
delete(myLib, scenario=c("one","two"),force=T)
scenario(myLib)

project(myLib)
delete(myLib,project=c(1,10),force=T)
project(myLib)
delete(myLib,project=c("copy","copy2"),force=T)
project(myLib)

#############################
# Datasheets
# Get datasheet from an SsimLibrary, Projects or Scenarios.
# Datasheets are provided in dataframe format
# We return lookup columns as factors, based on the definitions at the time the datasheet is created
# We also return each column in the correct data type.
delete("C:/Temp/NewLibrary.ssim",force=T)
myLibrary = ssimLibrary(name= "C:/Temp/NewLibrary.ssim",session=mySession)
scenario(myLibrary)
myScenario = scenario(myLibrary,scenario="one")
myProject = project(myLibrary,project=1)
myLibraryDataframes = datasheet(myLibrary, summary=F) # A named list of all the library datasheets.
#myScenarioDataframes = datasheet(myScenario,summary=F) #This takes a long time to run - so don't.
myProjectSheetNames = subset(datasheet(myProject),scope=="project") # A dataframe of datasheet names for project id 1.
names(myLibraryDataframes)
sheetName = "STime_Options"
mySTimeOptions = myLibraryDataframes[[sheetName]] # a single dataframe
# NOTE: datasheet(name=NULL,summary=F) can be very slow, and pull a lot of data - use rarely, with caution.

levels(mySTimeOptions$MultibandGroupingInternal)
mySTimeOptions[1,"MultibandGroupingInternal"]="Single band"
saveDatasheet(myProject,mySTimeOptions,name=sheetName) #Default is append=T for project/library scope datasheets, but data in single row datasheets is replaced, not appended, regardless of whether append=T
datasheet(myProject,sheetName) 

subset(datasheet(myScenario),scope=="scenario")$name
myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition")
myDeterministicTransitions = datasheet(myScenario,"STSim_DeterministicTransition",lookupsAsFactors=F) # This option returns characters instead of factors
#NOTE: this currently returns nothing because the sheet is empty and --colswithdata option does not include non-optional columns if the sheet is empty. Change behaviour in SyncroSim if desired.
#NOTE: option to return IDs rather than values for lookups postponed to a later version. See --export --rawvalues for support from SyncroSim.

# Get empty template dataframes - lookup columns are expressed as factors with only valid values
emptyDeterministicTransitionDataframe = datasheet(myScenario, name="STSim_DeterministicTransition",empty=T)

# Update the values for project datasheet - see commandLineTutorial for more examples.
sheetName = "STSim_StateLabelX"
delete(myProject,datasheet=sheetName,force=T)

stateClassDefinition = datasheet(myProject, name=sheetName,empty=F)
stateClassDefinition=addRow(stateClassDefinition,data.frame(Name=c('Coniferous','Deciduous','Mixed')))
# NOTE: rbind does not preserve types and factor levels
saveDatasheet(myProject, stateClassDefinition, name=sheetName) #append project scope datasheet by default
datasheet(myProject,sheetName)
stateClassDefinition=addRow(stateClassDefinition,data.frame(Name=c('Coniferous','Deciduous','Mixed','Grass')))
saveDatasheet(myProject, stateClassDefinition, name=sheetName) #Appends new record by default, but does not add redundant info
datasheet(myProject,sheetName)

#saveDatasheet(myProject, stateClassDefinition, name=sheetName,append=F) #prompt to approve removal of old definitions.
saveDatasheet(myProject, stateClassDefinition, name=sheetName,append=F,force=T) #remove without prompting
#if append = F, deletes existing project definitions. Note that deleting project definitions can also delete results and other things that depend on lookups. So prompt or require explicit approval.
#default is append=T for project/library scope datasheets, and F for scenario datasheets. 
#But note that data in single row datasheets is always replaced rather than appended, regardless of whether append=T. See documentation for details.

datasheet(myProject)
delete(myProject,datasheet=c(sheetName,sheetName),force=T)
datasheet(myProject, name=sheetName) 



#################
# Run
# Run a scenario and return the results scenario
# See commandLineTutorial.R for examples

########################
# Spatial data
# See STSimSpatialTutorial.R for examples.