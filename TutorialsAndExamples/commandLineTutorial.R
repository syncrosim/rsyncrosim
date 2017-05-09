# **********************************************************
# commandLineTutorial.R
# Following the steps in Leo's PowerShell script using rsyncrosim.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.15
# **********************************************************
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
#library(rsyncrosim)

#*************************************
# Create the project definition
myLibrary = SSimLibrary(model="stsim",name="C:/Temp/ST-Sim-Command-Line.ssim",forceUpdate=T)
myProject = project(myLibrary,name="ST-Sim Demonstration")

scenarios(myLibrary,names=T)
#***********************************
# Cover types and state classes
datasheets(myProject)
sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=T)
mySheet[1,"Name"]="Entire Forest"
#NOTE: this syntax preserves types and factor levels, and adds new rows if necessary. mySheet$Name="Entire Forest" does not.
loadDatasheets(myProject,mySheet,name=sheetName)
# NOTE: datasheet(), datasheets() and loadDatasheets() accept any combination of x, project and scenario arguments.
# x is a SyncroSim object (SsimLibrary,Project or Scenario) or name/path of a library on disk.
# scenario and project can be names, ids, or SycnroSim objects - loadDatasheets does not handle multiple projects/scenarios.
#
# NOTE: Default datasheet() retrieval (empty=F, stringsAsFactors=T) requires 2 console calls
# Setting empty=T eliminates one console call (or database query for some outputs)
# Setting stringsAsFactors=T eliminates a console call.
# Retrieval of output datasheets can be sped up by querying multiple scenarios (no extra calls),
# and only querying necessary information (using SELECT and GROUP BY sql statements).
# See examples below.

# Warns if lookups are not loaded, and returns a factor with 0 levels
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=F)
str(mySheet)
mySheet[1,"StateLabelYID"]="All" #A more cryptic warning because the factor has no levels.

sheetName = "STSim_StateLabelY"; mySheet = datasheet(myProject,name=sheetName)
mySheet[1,"Name"]="All"
?loadDatasheets
loadDatasheets(myProject,mySheet,name=sheetName)

sheetName = "STSim_StateLabelX"; mySheet = datasheet(myProject,name=sheetName,empty=F)
mySheet[1:3,"Name"]=c('Coniferous','Deciduous','Mixed')
loadDatasheets(myProject,mySheet,name=sheetName)

# Now lookups are loaded we can set StateClass
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"StateLabelXID"] ="hi" #Invalid value for a lookup column
mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
loadDatasheets(myProject,mySheet,name=sheetName)
#mySheet = datasheet(myProject,name=sheetName);str(mySheet)

# DISCUSS: lookups. Is this enough? If not, what else is needed?
# NOTE: special knowledge needed to construct Name here. - come back to this later.

#***********************************
# Transitions
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
datasheets(myProject,scope="project")$name #See project scope datasheet names
sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T,printCmd=T)
str(mySheet)
mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
loadDatasheets(myProject,mySheet,name=sheetName,printCmd=T)
loadDatasheets(myProject,subset(mySheet,select=Name),name="STSim_TransitionType")

sheetName = "STSim_TransitionTypeGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T)
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
# NOTE: To be consistent with project() I have used name/id in scenario().
datasheets(myScenario,scope="scenario")$name

#**************
# Run control
sheetName = "STSim_RunControl"; mySheet = datasheet(myScenario,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"MinimumIteration"] = 1
mySheet[1,"MaximumIteration"] = 40
mySheet[1,"MinimumTimestep"] = 0
mySheet[1,"MaximumTimestep"] = 50
loadDatasheets(myScenario,mySheet,name=sheetName)

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
loadDatasheets(myScenario,mySheet,name=sheetName)
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
mySheet=addRows(mySheet,data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20))
mySheet=addRows(mySheet,data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40))
mySheet=addRows(mySheet,data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40))
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
# Copies "No Harvest" scenario to new "Harvest" scenario

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
# DISCUSS: datasheets()
# When is it necessary/desireable to load all datasheets?
# Is minimal syntax (e.g. myDatasheets[["STSim_StateClass"]]) the main goal?
# If so, we could define a Datasheets object that contains datasheet names and info required for retreival (libraryPath/Session/scenarioIds/projectIds).
# We could then overwrite names(), [[]] to get list-like behaviour. [[]] would get the datasheet from SyncroSim.

showMethods(class="SsimLibrary",where=loadNamespace("rsyncrosim")) # See methods for the Session object
anotherProject = project(myLibrary,name="AnotherProject")
# DISCUSS: Inheritance
# Project and Scenario objects inherit from SsimLibrary.
# For some methods, this is helpful:
#  - datasheet(), datasheets(), loadDatasheets(): do sensible things with x/project/scenario arguments - see help for details.
#  - run(): does sensible things with x/scenario arguments
#  - session(), modelVersion(), modelName(), filepath(), addons(): provide useful information.
#
# Other methods are conceptually problematic and should (?) be disabled for Scenario/Project objects.
#  - enableAddons<-,disableAddons<- : side effects for other projects/scenarios
#  - deleteScenarios(): only let a parent (Project or SsimLibrary) delete a scenario?
#  - deleteProjects(): only let parent SsimLibrary delete a project?
#  - projects(): only SSimLibraries can contain more than one project.
#  - scenarios(): only SSimLibraries and Projects can contain more than one scenario.
#
# And I am unsure about these methods:
#  - info(): returns library info
#  - session<-: When should users be allowed to change the version of SyncroSim they are using?

myScenarios = scenarios(myProject) #returns list - names are scenario ids.
names(myScenarios)
# NOTE: base R function names returns id's, not scenario names. I don't recommend overwriting the base function for List objects.

deleteProjects(myLibrary, project="My new project name") # Returns a list of "saved" or a failure messages for each project.
# QUESTION: Do we want to be consistent about "project" vs "projects" here?
# QUESTION: consistency with enable/disableAddons? When should I use assignment operators?
# QUESTION: generic delete method?

# QUESTION: Default names for new projects and scenarios???

parentId(myScenario)
# QUESTION: Should I disable assignment functions for result scenarios?

# DISCUSS: What exactly is a datasheet object, and why do we need one?

# DISCUSS: StochasticTime chart and map UI - What features do we need?

# DISCUSS: Examples in help files - some examples are difficult to set up.

# DISCUSS: How to acknowledge code bits scooped from web pages? search 'http' to see them...


################
# TO DO:
# - handle raster datasheets (inputs)
# - datasheet(,keepId=T)
# - help/documentation
# - Project revisions: Safe modification of existing libraries?
# - long names in  models() ?
# - Dependency functions: command(list(create=NULL,dependency=NULL,lib=.filepath(myLibrary),sid=.id(myScenario),did=.id(myDependency)))
# - warning: loadDatasheet() appends to library and project scope datasheets, rather than overwriting. To start fresh, delete project or library and begin again. We likely need a better way to modify project scope datasheets...
# - Note - I spend a lot of time trying to figure out how GUI naming corresponds to datasheet names here.
#   e.g. I need Transition Pathways -> Transitions. What sheet is that, exactly?
#   Solution: subset(datasheets(myScenario,names=T),displayName=="Transitions",select=c(name))
# - Copy a scenario from one project to another?
# - Check which datasheets have data.
# - How to access results more efficiently. E.G. Transitions table.
# - More graceful failure given insufficient version of SyncroSim. Colin got a wierd failure error (indexing?) from 0.24.
# - Problems with default transition type groups:
#    sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,empty=T,printCmd=T)
#    mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
#    loadDatasheets(myProject,mySheet,name=sheetName,printCmd=T)
#   I've added an "IsVisible" column property to --list --columns so Josie can check for this as well.
#   We should be careful about omitting fields for datasheets on export because when you import a datasheet it updates existing records with the new data.  For example, if you omit the "Description" field then existing description will be overwritten with NULL if you import that same file.  I'm not sure if this is what is happening in the R scripts, but just something to be aware of.  Of course we could change things so data is never overwritten with NULL for definition imports (and Cut/Paste...) but then you would not be able to replace an existing definition with NULL which seems a bit wrong.
#   Ensure that factor levels are passed through from invisible datasheet
# - Add index page to reference manual.
# - resolve id() conflict between dplyr and rsyncrosim.
# - loadSpatialData() when breakpoint=F. See A76/ChangeResolutionOfInputs.R. But note there may be more elegant ways to do this.
# - bug in datasheet(). See line 43 of A176/ChangeTimestep.R
# - flag for Alex. If the dependency scenario does not exist, syncrosim creates a blank scenario. command(list(create=NULL,dependency=NULL,lib=filepath(myProject),sid=id(newRunScenario),did=id(newInitialConditions)))
# - better errors from syncrosim - display log entries by default
# - saveDatasheet(): handle named vectors
# - datasheet(): return named vector for single row datasheets
# - datasheets(): default is object scope
# - combine datasheet() and datasheets()? Reconsider plurals more generally.
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
