# **********************************************************
# commandLineTutorial.R
# Following the steps in Leo's PowerShell script using rsyncrosim.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.15
# **********************************************************
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
#library(rsyncrosim)
mySession = session(defaultModel="carep") #note different options in SyncroSim v2
defaultModel(mySession)
defaultModel(mySession)="stsim"
defaultModel(mySession)

modules(mySession)
#deleteModule("sample-basic-dotnet") #using default session
#modules(mySession)
#deleteModule(c("sample-stime-dotnet","sample-spatial-dotnet"),mySession) #a vector, using a particular session
#modules(mySession)

#Three different ways to provide args to command
command(c("create","help"))
command("--create --help",session=session(printCmd=T))
command(list(create=NULL,help=NULL))

args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp55.ssim"),model="hello:model-transformer")
output = command(args,session=session(printCmd=T,silent=F))

mySession =session(printCmd=T,silent=F)
#delete(paste0(getwd(),"/temp26.ssim"),force=T)
#delete(paste0(getwd(),"/temp27.ssim"),force=T)

myLib=ssimLibrary(name="temp26",session=mySession)
addons(myLib)
addons(mySession)
datasheet(myLib)
delete(myLib,force=T)
#project(myLib) #Fails with message that library does not exist on disk.
myLib=ssimLibrary(name="temp26",session=mySession)
myOtherLib = ssimLibrary(name="temp27",session=mySession)
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

myOtherScn = scenario(myOtherLib,scenario="other")
scenario(myOtherLib)
#delete("myOtherLib",scenario="other",force=T)#Fails if library does not exist
delete(myOtherLib,scenario="other",force=T)
myOtherScn = scenario(myOtherLib,scenario="other2")

project(myOtherLib)
scenario(myOtherLib)

myProject = project(myLib,project="temp")
datasheet(myProject) #Note hasData info only available for scenario scope datasheets.
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
datasheet(myLib,project="temp") #same thing, but more system calls.
project(myLib)

#scenario(myLib,scenario=1) # Fail: need a name to create a scenario
myScn = scenario(myLib,scenario="one") #Ok because only one project in the library.
scenario(myLib)
project(myLib)
myProject = project(myLib,project="temp2")
myScn = scenario(myLib,scenario="one") #Ok because only one scenario of this name occurs in the library.
#delete(myProject,scenario="one",force=T)
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

sheets = datasheet(myScn) #only scope, name and displayName
sheets 
sheets = datasheet(myScn,optional=T) #all info
str(sheets)
#NOTE: hasData column only available for scenario scope datasheets
#NOTE: dataInherited and dataSource columns added if there are dependencies. 

scenario(myLib)
projectId(myProject)
#delete(myProject,scenario="two",force=T)
myScn = scenario(myProject,scenario="one",sourceScenario="one") #Ok because only one possible source
myScn = scenario(myProject,scenario="one",sourceScenario="one") #Warns that sourceScenario will be ignored.
#myScn = scenario(myProject,scenario="two",sourceScenario="one") #Fail if more than one scenario named sourceScenario in the library.
scenarioId(myScn)
scenario(myScn,summary=T) #return summary info

allSheets=datasheet(myScn,optional=T)#returns all info about datasheets
str(allSheets)
allSheets = datasheet(myScn) #returns only datasheet names and scope
allSheets

# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

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

name(myLib)
name(myLib)="Fred"
name(myLib) 
filepath(myLib)#Note that the filename on disk has not changed

backup(myLib)
description(myLib) = "A new description.\nTry a linebreak." #NOTE: \n adds a linebreak to the description
description(myLib) #QUESTION: Each element of the vector is a line of the description. Should this change?
owner(myLib) ="Fred"
owner(myLib)
readOnly(myLib)=T
readOnly(myLib)
readOnly(myLib)=F
readOnly(myLib) 
addons(myLib) #TO DO: test disableAddon() and enableAddon() once we have some addons.

model(mySession)
modules(mySession) #NOTE: model(mySession) is not a subset of modules(mySession). Change in SyncroSim if necessary.
model()
modules()
model(myLib)

version(mySession)
version()
myScn = scenario(myProject,scenario=1)
runLog(myScn) #Returns message if the scenario is not a result scenario.
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

command("--help")

#TO DO: test runLog() on result scenario
#TO DO: once we have addons - is shortName necessary?
#TO DO: test disableAddon() and enableAddon() once we have some addons.
#TO DO: allow setting readOnly=F from T.
#TO DO: go over STSimSpatialTutorial.R newScenario part with Alex. What is needed to set spatial inputs in a new library?
#TO DO: test delete().
#TO DO: session()  Use version() properly once that function is updated. 
#TO DO: for saveDatasheet() handle data without names.
#TO DO: test run in general, and given a list of objects, given forceElements=F, given summary=T.
#TO DO: revise datasheet() given new options from --export: --extfilepaths --rawvalues
#TO DO: test datasheet() optional=F, empty=F with database queries (output for multiple scenarios)
#TO DO: test datasheet() for scenario with dependencies.
#TO DO: complain if delete library fails because file is locked.

#*************************************
# Create the project definition
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

myLibrary = ssimLibrary(name="C:/Temp/ST-Sim-Command-Line.ssim",forceUpdate=T,session=mySession)
myProject = project(myLibrary,project="ST-Sim Demonstration")
delete(myProject,force=T)
myProject = project(myLibrary,project="ST-Sim Demonstration")

#***********************************
# Cover types and state classes
datasheet(myProject)
sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=F,optional=T)
mySheet[1,"Name"]="Entire Forest"
mySheet[1,"Description"]="Another description"
#NOTE: this syntax preserves types and factor levels, and adds new rows if necessary. mySheet$Name="Entire Forest" does not.
saveDatasheet(myProject,mySheet,name=sheetName)

datasheet(myProject,name=sheetName,empty=T,optional=F) #returns only truly optional columns
datasheet(myProject,name=sheetName,empty=F,optional=F) #returns optional columns and columns with data
datasheet(myProject,name=sheetName,empty=F,optional=T) #returns all columns

# Warns if lookups are not loaded, and returns a factor with 0 levels
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=F)
str(mySheet)
mySheet[1,"StateLabelYID"]="All" #A more cryptic warning because the factor has no levels.

sheetName = "STSim_StateLabelY"; mySheet = datasheet(myProject,name=sheetName)
mySheet[1,"Name"]="All"
saveDatasheet(myProject,mySheet,name=sheetName)

#include optional columns
sheetName = "STSim_StateLabelX"; mySheet = datasheet(myProject,name=sheetName,empty=T,optional=T) 
mySheet[1:2,"Name"]=c('Coniferous','Deciduous')
mySheet[1:2,"Description"]=c('coniferous forest','deciduous forest')
saveDatasheet(myProject,mySheet,name=sheetName)
datasheet(myProject,name=sheetName)

#by default, saveDatasheet appends project/library scope datasheets
mySheet =  setNames(c('Mixed'), c("Name")) #A named vector
saveDatasheet(myProject,mySheet,name=sheetName) 
datasheet(myProject,name=sheetName)

#overwrite existing values
mySheet=list(Name=c('Coniferous','Deciduous','Mixed'))
saveDatasheet(myProject,mySheet,name=sheetName,append=F) 
datasheet(myProject,name=sheetName)

# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

# Now lookups are loaded we can set StateClass
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"StateLabelXID"] ="hi" #Invalid value for a lookup column
mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
saveDatasheet(myProject,mySheet,name=sheetName)
#mySheet = datasheet(myProject,name=sheetName);str(mySheet)

datasheet(myProject,sheetName,lookupsAsFactors = F)
datasheet(myProject,sheetName,lookupsAsFactors = T,optional=T,includeKey=T)
# NOTE: special knowledge needed to construct Name here. - come back to this later.

#***********************************
# Transitions
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
subset(datasheet(myProject),scope=="project")$name #See project scope datasheet names
sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
saveDatasheet(myProject,mySheet,name=sheetName)
saveDatasheet(myProject,subset(mySheet,select=Name),name="STSim_TransitionType")

sheetName = "STSim_TransitionTypeGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T)
mySheet[1:3,"TransitionTypeID"]=levels(mySheet$TransitionTypeID)
mySheet[1:3,"TransitionGroupID"]=levels(mySheet$TransitionGroupID)
saveDatasheet(myProject,mySheet,name=sheetName)

#****************
# Age type
sheetName = "STSim_AgeType"; mySheet = datasheet(myProject,name=sheetName,empty=F)
datasheet(myProject,name=sheetName,summary=T,optional=T) #get info about this sheet

str(mySheet)
mySheet[1,"Frequency"] = 1
mySheet[1,"MaximumAge"] = 100
saveDatasheet(myProject,mySheet,name=sheetName)

#*************************************
# Add No Harvest Scenario
#*************************************
myScenario = scenario(myProject,scenario="No Harvest")
subset(datasheet(myScenario),scope=="scenario")$name

#**************
# Run control
sheetName = "STSim_RunControl"; mySheet = datasheet(myScenario,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"MinimumIteration"] = 1
mySheet[1,"MaximumIteration"] = 40
mySheet[1,"MinimumTimestep"] = 0
mySheet[1,"MaximumTimestep"] = 50
saveDatasheet(myScenario,mySheet,name=sheetName)

#**************************
# Deterministic transitions
# devtools::document();devtools::load_all()
sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
levels(mySheet$StateClassIDSource)
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1"))
mySheet=addRows(mySheet,list(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1"))
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1"))
mySheet
saveDatasheet(myScenario,mySheet,name=sheetName)
# mySheet = datasheet(myScenario,name=sheetName,optional=T); str(mySheet) #check what happened
# addRows() checks validity of column names and factor levels.
# addRows() fills missing values using factor levels where possible.

#*************************
# Probabilistic transitions
sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.01))
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Harvest",Probability=1,AgeMin=40))
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.002))
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=10))
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.005))
mySheet=addRows(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=20))
mySheet
saveDatasheet(myScenario,mySheet,name=sheetName)
#mySheet = datasheet(myScenario,name=sheetName,optional=T); mySheet #check what happened

#********************
#Initial conditions
# devtools::document();devtools::load_all()
sheetName = "STSim_InitialConditionsNonSpatial"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
mySheet[1,"TotalAmount"]=1000
mySheet[1,"NumCells"]=1000
saveDatasheet(myScenario,mySheet,name=sheetName)

sheetName = "STSim_InitialConditionsNonSpatialDistribution"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
mySheet=addRows(mySheet,data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20))
mySheet=addRows(mySheet,data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40))
mySheet=addRows(mySheet,data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40))
mySheet
saveDatasheet(myScenario,mySheet,name=sheetName)

#******************
# Transition targets
sheetName = "STSim_TransitionTarget"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
str(mySheet)
mySheet[1,"TransitionGroupID"]="Harvest"
mySheet[1,"Amount"]=0
saveDatasheet(myScenario,mySheet,name=sheetName)

#*************************************
# Add Harvest Scenario
#*************************************
# devtools::document();devtools::load_all()
# delete(myProject,"Harvest",force=T)
myScenario = scenario(myProject,scenario="Harvest",sourceScenario="No Harvest")
# Copies "No Harvest" scenario to new "Harvest" scenario

#******************
# Transition targets
sheetName = "STSim_TransitionTarget"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
str(mySheet)
mySheet[1,"TransitionGroupID"]="Harvest"
mySheet[1,"Amount"]=20
saveDatasheet(myScenario,mySheet,name=sheetName)

#********************************
# Run scenarios
#******************************
# devtools::document();devtools::load_all()

myResults = run(myProject,scenario=c("Harvest","No Harvest"),jobs=4)
# By default, returns a named list of result Scenario objects.
# If onlyIds = TRUE (slightly faster), returns result scenario ids instead of objects
# NOTE: jobs is passed through to SyncroSim which handles multithreading.

scenario(myProject)

# delete(myProject,3,force=T)
# myResults=scenario(myProject,summary=F,results=T)

#********************************
# See results
#******************************
# devtools::document();devtools::load_all()
outStates = datasheet(myResults,name="STSim_OutputStratumState")
str(outStates)
unique(outStates$ScenarioParent)
# NOTE: For outputs, use lookupsAsFactors=T
# Output table lookups are IDs in the database, rather than labels - not true for input tables.
#
# NOTE: can query multiple projects or scenarios - see ?datasheet for details.
# Requires 1 database query and 1 console call (+lookup queries), regardless of the number of scenarios included in myResults
#
# NOTE: We can also query the database more precisely to avoid pulling unecessary information.
# There are >400,000 records in this small example.
sheetName = "STSim_OutputStratumState"
names(datasheet(myResults,name=sheetName,lookupsAsFactors=F,empty=T)) #Get column names without getting data
unique(outStates$Iteration)

mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","StateLabelXID"),aggregate=c("Amount"),where=list(Timestep=c(0,1,2),Iteration=c(3,4)))
mySQL # A list of SELECT, WHERE and GROUP BY SQL statements passed to SQLite.
outStatesAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
str(outStatesAllAges) #Much faster: fewer lookups and fewer records.
sheetName = "STSim_OutputStratumTransition"
names(datasheet(myResults,name=sheetName,lookupsAsFactors=F,empty=T)) #Get column names without getting any data
mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","TransitionGroupID"),aggregate=c("Amount"))
outTransitionsAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
str(outTransitionsAllAges)

# NOTE: In the following example we use existing R tools (ggplot2/plyr) to visualize the output.
# install.packages("ggplot2");install.packages("plyr")
library(ggplot2);library(plyr)

#Example visualization - mean and 95% confidence bands for area in each state over time.
outStatesSummary = ddply(outStatesAllAges,.(Timestep,StateLabelXID,ScenarioParent),summarize,amount=mean(Amount),upperAmount=quantile(Amount,0.975),lowerAmount=quantile(Amount,0.025))
base = ggplot(outStatesSummary,aes(x=Timestep,y=amount,ymax=upperAmount,ymin=lowerAmount))+geom_line()+geom_ribbon(alpha=0.1)
base=base+facet_grid(StateLabelXID~ScenarioParent)+ theme_bw()
base=base+ylab("area (acres)")
print(base)

#Example visualization - mean and 95% confidence bands for transitions over time.
outTransitionsSummary = ddply(outTransitionsAllAges,.(Timestep,TransitionGroupID,ScenarioParent),summarize,amount=mean(Amount),upperAmount=quantile(Amount,0.975),lowerAmount=quantile(Amount,0.025))
base = ggplot(outTransitionsSummary,aes(x=Timestep,y=amount,ymax=upperAmount,ymin=lowerAmount))+geom_line()+geom_ribbon(alpha=0.1)
base=base+facet_grid(TransitionGroupID~ScenarioParent,scales="free_y")+ theme_bw()
base=base+ylab("area (acres)")
print(base)

#*********************
# TO DISCUSS
#*********************
showMethods(class="SsimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
anotherProject = project(myLibrary,project="AnotherProject")
# DISCUSS: Inheritance
# Project and Scenario objects inherit from SsimLibrary.
# For some methods, this is helpful:
#  - datasheet(), saveDatasheet(): do sensible things with x/project/scenario arguments - see help for details.
#  - run(): does sensible things with ssimObject/scenario arguments
#  - session(), filepath(), addons(): provide useful information.
#
# Other methods are conceptually problematic and should (?) be disabled for Scenario/Project objects.
#  - enableAddon,disableAddon : side effects for other projects/scenarios
#
# And I am unsure about these methods:
#  - info(): returns library info
#  - session<-: When should users be allowed to change the version of SyncroSim they are using?

myScenarios = scenario(myProject,summary=F) #returns list - names are scenario ids.
names(myScenarios)
# NOTE: base R function names returns id's, not scenario names. I don't recommend overwriting the base function for List objects.

delete(myLibrary, project="My new project name") # Returns "saved" or a failure message, or a list of these for each project.

parentId(myScenario)
# QUESTION: Should I disable assignment functions for result scenarios?

# DISCUSS: StochasticTime chart and map UI - What features do we need?

# DISCUSS: Examples in help files - some examples are difficult to set up.

# DISCUSS: How to acknowledge code bits scooped from web pages? search 'http' to see them...


################
# TO DO:
# - handle raster datasheets (inputs)
# - datasheet(,keepId=T)
# - help/documentation
# - Project revisions: Safe modification of existing libraries?
# - long names in  model() ?
# - Dependency functions: command(list(create=NULL,dependency=NULL,lib=.filepath(myLibrary),sid=.scenarioId(myScenario),did=.projectId(myDependency)))
# - warning: loadDatasheet() appends to library and project scope datasheets, rather than overwriting. To start fresh, delete project or library and begin again. We likely need a better way to modify project scope datasheets...
# - Note - I spend a lot of time trying to figure out how GUI naming corresponds to datasheet names here.
#   e.g. I need Transition Pathways -> Transitions. What sheet is that, exactly?
#   Solution: subset(datasheet(myScenario),displayName=="Transitions",select=c(name))
# - Copy a scenario from one project to another?
# - Check which datasheets have data.
# - How to access results more efficiently. E.G. Transitions table.
# - More graceful failure given insufficient version of SyncroSim. Colin got a wierd failure error (indexing?) from 0.24.
# - Problems with default transition type groups:
#    sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T)
#    mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
#    saveDatasheet(myProject,mySheet,name=sheetName)
#   I've added an "IsVisible" column property to --list --columns so Josie can check for this as well.
#   We should be careful about omitting fields for datasheets on export because when you import a datasheet it updates existing records with the new data.  For example, if you omit the "Description" field then existing description will be overwritten with NULL if you import that same file.  I'm not sure if this is what is happening in the R scripts, but just something to be aware of.  Of course we could change things so data is never overwritten with NULL for definition imports (and Cut/Paste...) but then you would not be able to replace an existing definition with NULL which seems a bit wrong.
#   Ensure that factor levels are passed through from invisible datasheet
# - Add index page to reference manual.
# - loadSpatialData() when breakpoint=F. See A76/ChangeResolutionOfInputs.R. But note there may be more elegant ways to do this.
# - bug in datasheet(). See line 43 of A176/ChangeTimestep.R
# - better errors from syncrosim - display log entries by default
# - saveDatasheet(): handle named vectors
# - datasheet(): return named vector for single row datasheets
# - saveDatasheet() instead of loadDatasheet()
# - delete datasheet - start again. Currently can overwrite, but cannot start from scratch.

####################
#DONE
# - Reconcile SyncroSim and R colors: Syncrosim puts the alph in the first position, R expects it in the last position.
#   rat$Color = sapply(rat$Color,fixSyncroSimColors,simplify=T)
#   fixSyncroSimColors<-function(inCol){
#     inVals = strsplit(inCol,",")[[1]]
#     outVals=inVals
#     outVals[1:3]=inVals[2:4]
#     outVals[4]=inVals[1]
#     return(paste(outVals,collapse=","))
#   }
# - Update plotting examples. This code will assign the right colors to the right classes
#   view=stateClass
#   myCols = unique(subset(levels(view)[[1]],select=c(Name,hexColor,Color)));myCols=myCols[order(myCols$Name),]
#   levelplot(view,att="Name",at=myCols$Name,col.regions=myCols$hexColor,par.settings=myCols,main=view@title)
# - Used "Saved" instead of "Success", where appropriate


###################
# Handoff notes:
# - Clean up namespace. Don't export internalWrapper.R functions. Scan for other unnecessary exports.
# - Review help files.
# - Put examples in help files. Figure out what to do about difficult examples.
# - linux testing
# - vanilla windows testing
# - search and handle "TO DO:" notes

###############
# LOW PRIORITY
# LOW PRIORITY: Platform agnostic paths. For now, ask Linux users to specify the path to SyncroSim.Console.exe
# LOW PRIORITY: Better explain command with help examples: c("list","models")
# LATER: Create own model from scratch in R. Inputs, output and calculations
# Backup and restore libraries.
# LOW PRIORITY - datafeeds
# LOW PRIORITY: Defaults for primary and secondary strata in STSim models?
