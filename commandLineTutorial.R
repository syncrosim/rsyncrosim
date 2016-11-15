# **********************************************************
# commandLineTutorial.R
# Following the steps in Leo's PowerShell script using rsyncrosim.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.10
# **********************************************************
# devtools::document();devtools::load_all()

#*************************************
# Create the project definition
myLibrary = ssimLibrary(model="stsim",name="C:/Temp/ST-Sim-Command-Line.ssim")
myProject = project(myLibrary,name="ST-Sim Demonstration")

#***********************************
# Cover types and state classes
datasheets(myProject,names=T)
sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=T)
mySheet[1,"Name"]="Entire Forest"
loadDatasheets(myProject,mySheet,name=sheetName)
# NOTE: datasheet(), datasheets() and loadDatasheets() accept any combination of x, project and scenario arguments.
# x is a SyncroSim object (SSimLibrary,Project or Scenario) or name/path of a library on disk.
# scenario and project can be names, ids, or SycnroSim objects - loadDatasheets does not handle multiple projects/scenarios.
#
# NOTE: Default datasheet() retrieval (empty=F, stringsAsFactors=T) requires a database query and at least 1 console call
# A console call is also required for each dependency, so the default datasheet() can be slow.
# Setting empty=T eliminates the database query.
# Setting stringsAsFactors=T eliminates the console call.
# Getting a datasheet for multiple scenarios or projects requires only 1 extra console call.
# Do we need more options for speeding the process?

# Warns if dependencies are not loaded, and return a factor with 0 levels
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
mySheet[1,"StateLabelYID"]="All" #A more cryptic warning because the factor has no levels.

sheetName = "STSim_StateLabelY"; mySheet = datasheet(myProject,name=sheetName)
mySheet[1,"Name"]="All"
loadDatasheets(myProject,mySheet,name=sheetName)

sheetName = "STSim_StateLabelX"; mySheet = datasheet(myProject,name=sheetName,empty=T)
mySheet[1:3,"Name"]=c('Coniferous','Deciduous','Mixed')
loadDatasheets(myProject,mySheet,name=sheetName)

# Now dependencies are loaded we can set StateClass
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"StateLabelXID"] ="hi" #Invalid value for a column with dependency
mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
loadDatasheets(myProject,mySheet,name=sheetName)
#mySheet = datasheet(myProject,name=sheetName);str(mySheet)

# DISCUSS: dependencies. Is this enough? If not, what else is needed?
# DISCUSS: special knowledge needed to construct Name here?

#***********************************
# Transitions
datasheets(myProject,names=T,scope="project")$name #See project scope datasheet names
sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T)
mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
loadDatasheets(myProject,mySheet,name=sheetName)
loadDatasheets(myProject,mySheet,name="STSim_TransitionType")

sheetName = "STSim_TransitionTypeGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1:3,"TransitionTypeID"]=levels(mySheet$TransitionTypeID)
mySheet[1:3,"TransitionGroupID"]=levels(mySheet$TransitionGroupID)
loadDatasheets(myProject,mySheet,name=sheetName)

#****************
# Age type
sheetName = "STSim_AgeType"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"Frequency"] = 1
mySheet[1,"MaximumAge"] = 100
loadDatasheets(myProject,mySheet,name=sheetName)

#*************************************
# Add No Harvest Scenario
#*************************************
myScenario = scenario(myProject,name="No Harvest")
datasheets(myScenario,names=T,scope="scenario")$name

#**************
# Run control
sheetName = "STSim_RunControl"; mySheet = datasheet(myScenario,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"MinimumIteration"] = 1
mySheet[1,"MaximumIteration"] = 40
mySheet[1,"MaximumTimestep"] = 0
mySheet[1,"MaximumTimestep"] = 50
loadDatasheets(myScenario,mySheet,name=sheetName)

#**************************
# Deterministic transitions
# devtools::document();devtools::load_all()
sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
levels(mySheet$StateClassIDSource)
addRows(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1")
addRows(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1")
addRows(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1")
mySheet
loadDatasheets(myScenario,mySheet,name=sheetName)
# mySheet = datasheet(myScenario,name=sheetName,optional=T); str(mySheet) #check what happened
# addRows() checks validity of column names and factor levels.
# addRows() fills missing values using factor levels where possible.

#*************************
# Probabilistic transitions
sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
addRows(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.01)
addRows(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Harvest",Probability=1,AgeMin=40)
addRows(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.002)
addRows(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=10)
addRows(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.005)
addRows(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=20)
mySheet
loadDatasheets(myScenario,mySheet,name=sheetName)
#mySheet = datasheet(myScenario,name=sheetName,optional=T); mySheet #check what happened

#********************
#Initial conditions
# devtools::document();devtools::load_all()
sheetName = "STSim_InitialConditionsNonSpatial"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
mySheet[1,"TotalAmount"]=1000
mySheet[1,"NumCells"]=1000
loadDatasheets(myScenario,mySheet,name=sheetName)

sheetName = "STSim_InitialConditionsNonSpatialDistribution"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
addRows(mySheet)=data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20)
addRows(mySheet)=data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40)
addRows(mySheet)=data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40)
mySheet
loadDatasheets(myScenario,mySheet,name=sheetName)

#******************
# Transition targets
sheetName = "STSim_TransitionTarget"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
str(mySheet)
mySheet[1,"TransitionGroupID"]="Harvest"
mySheet[1,"Amount"]=0
loadDatasheets(myScenario,mySheet,name=sheetName)

#*************************************
# Add Harvest Scenario
#*************************************
# devtools::document();devtools::load_all()
# deleteScenarios(myProject,"Harvest",force=T)
myScenario = scenario(myProject,name="Harvest",sourceScenario="No Harvest")
# Copy "No Harvest" scenario to new "Harvest" scenario

#******************
# Transition targets
sheetName = "STSim_TransitionTarget"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
str(mySheet)
mySheet[1,"TransitionGroupID"]="Harvest"
mySheet[1,"Amount"]=20
loadDatasheets(myScenario,mySheet,name=sheetName)

#********************************
# Run scenarios
#******************************
# devtools::document();devtools::load_all()

myResults = run(myProject,scenario=c("Harvest","No Harvest"),jobs=4)
# By default, returns a named list of result Scenario objects.
# If onlyIds = TRUE (slightly faster), returns result scenario ids instead of objects
# NOTE: jobs is passed through to SyncroSim which handles multithreading.

scenarios(myProject,names=T)

# deleteScenarios(myProject,3,force=T)
# myResults=scenarios(myProject,results=T)

#********************************
# See results
#******************************
# devtools::document();devtools::load_all()
outStates = datasheet(myResults,name="STSim_OutputStratumState")
str(outStates)
unique(outStates$ScenarioParent)
# NOTE: querying dependencies here is slow (1 database query per dependendency) but necessary -
# Output table dependencies are IDs in the database, rather than labels - not true for input tables.
# This would be much faster if the database held labels, rather than IDs
#
# NOTE CHANGE: can query multiple projects or scenarios - see ?datasheet for details.
# Requires 1 database query and 1 console call, regardless of the number of scenarios included in myResults
#
# NOTE: We can also query the database more precisely to avoid load unecessary information.
# This will help avoid trouble with very large tables. >400,000 records in this small example.
sheetName = "STSim_OutputStratumState"
varNames = names(datasheet(myResults,name=sheetName,dependsAsFactors=F,empty=T)) #Get column names without getting any data
varNames #see column names
mySQL = sqlStatements(varNames,drop=c("AgeMin","AgeMax","AgeClass"),aggregate=c("Amount"))
mySQL # A list of SELECT and GROUP BY SQL statements passed to SQLite. Adventuruous users can get creative.
outStatesAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
str(outStatesAllAges)

sheetName = "STSim_OutputStratumTransition"
varNames = names(datasheet(myResults,name=sheetName,dependsAsFactors=F,empty=T)) #Get column names without getting any data
mySQL = sqlStatements(varNames,drop=c("AgeMin","AgeMax","AgeClass"),aggregate=c("Amount"))
outTransitionsAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
str(outTransitionsAllAges)

# NOTE: In the following example I use existing R tools (ggplot2/plyr) to visualize the output.
# DISCUSS: What (if any) summary/visualization functions do we need in rsyncrosim?
# install.packages("ggplot2");install.packages("plyr")
library(ggplot2);library(plyr)
# QUESTION: what to do about id() masking by plyr?

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
# devtools::document();devtools::load_all()
# NOTE: At present we depend on DBI and RSQLite (which is a Wickham package)

# NOTE: To export a datasheet we query the database directly. Otherwise, we use the console.

# DISCUSS: datasheets()
# When is it necessary/desireable to load all datasheets?
# Is minimal syntax (e.g. myDatasheets[["STSim_StateClass"]]) the main goal?
# If so, we could define a Datasheets object that contains datasheet names and info required for retreival (libraryPath/Session/scenarioIds/projectIds).
# We could then overwrite names(), [[]] to get list-like behaviour. [[]] would get the datasheet from SyncroSim.

# DISCUSS: StochasticTime chart and map UI
# What features do we need?

showMethods(class="SSimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
anotherProject = project(myLibrary,name="AnotherProject")
# DISCUSS: Inheritance
# Project and Scenario objects inherit from SSimLibrary.
# For some methods, this is helpful:
#  - datasheet(), datasheets(), loadDatasheets(): do sensible things with x/project/scenario arguments - see help for details.
#  - run(): does sensible things with x/scenario arguments
#  - session(), modelVersion(), modelName(), filepath(), addons(): provide useful information.
#
# Other methods are conceptually problematic and should (?) be disabled for Scenario/Project objects.
#  - enableAddons<-,disableAddons<- : side effects for other projects/scenarios
#  - deleteScenarios(): only let a parent (Project or SSimLibrary) delete a scenario?
#  - deleteProjects(): only let parent SSimLibrary delete a project?
#  - projects(): only SSimLibraries can contain more than one project.
#  - scenarios(): only SSimLibraries and Projects can contain more than one scenario.
#
# And I am unsure about these methods:
#  - info(): returns library info
#  - session<-: When should users be allowed to change the version of SyncroSim they are using?

myScenarios = scenarios(myProject) #returns list - names are scenario ids.
names(myScenarios)
# DISCUSS: base R function names returns id's, not scenario names. I don't recommend overwriting the base function for List objects.

deleteProjects(myLibrary, project="My new project name") # Returns a list of "Success!" or a failure messages for each project.
# QUESTION: Do we want to be consistent about "project" vs "projects" here?
# QUESTION: consistency with enable/disableAddons? When should I use assignment operators?
# QUESTION: generic delete method?

myScenario = scenario(myLibrary)
# QUESTION: In what cases do we want this to work?
# At present a project is required to create a scenario
# Ideas: if no project, create project/scenario?
# Fail if more than one project.

# QUESTION: Default names for new projects and scenarios???

myScenario=scenario(myProject,name="Harvest")
# NOTE: To be consistent with project() I have used name/id in scenario().

parentId(myScenario)
# QUESTION: Should I disable assignment functions for result scenarios?

# DISCUSS: What exactly is a datasheet object, and why do we need one?

# DISCUSS addRow<-: should this be an assignment function or a normal function?

# DISCUSS blanks and NA values in datasheets: I have handled the cases in this tutorial. What else gave you trouble?

################
# TO DO:
# - update()
# - scenario(myLibrary): handle case with only one project
# - datasheet(,keepId=T)
# - get/set summary information (name,author,description,readOnly): Alex is working on this.
# - handle raster datasheets (input and output)
# - tests
# - bigger data example.
# - Project revisions: Safe modification of existing libraries?
# - break points
# - help/documentation
# - ??

###############
# LOW PRIORITY
# LOW PRIORITY: Platform agnostic paths. For now, ask Linux users to specify the path to SyncroSim.Console.exe
# LOW PRIORITY: Better explain command with help examples: c("list","models")
# LATER: Create own model from scratch in R. Inputs, output and calculations
# Backup and restore libraries.
# LOW PRIORITY - datafeeds
# LOW PRIORITY: Defaults for primary and secondary strata in STSim models?
