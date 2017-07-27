# **********************************************************
# commandLineTutorial.R
# Create a simple non-spatial stsim model using rsyncrosim.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.15
# **********************************************************
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
#library(rsyncrosim)

#*************************************
# Create the project definition
sessionPath = "c:/gitprojects/syncrosim/_deploy_/current" #Note default session won't work until we have a real release of SyncroSim v2
libPath = "C:/Temp/ST-Sim-Command-Line.ssim"
delete(libPath,force=T) #remove old version of the library (if any). Start fresh.
myLibrary = ssimLibrary(name=libPath,session=session(sessionPath)) #create a new library
myProject = project(myLibrary,project="ST-Sim Demonstration") #create a new project

#str(datasheet(myProject,"STSim_StateClass"))
#forestTypes = data.frame(Name=c("Coniferous", "Deciduous", "Mixed"))
#stateClasses = forestTypes
#stateClasses$StateLabelYID = "All"
#str(stateClasses)
#saveDatasheet(myProject, stateClasses, "STSim_StateClass", append = FALSE,force=T)
#"The data contains a NULL for 'Forest Type'."

#***********************************
# Cover types and state classes
datasheet(myProject)
sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=F,optional=T)
mySheet[1,"Name"]="Entire Forest"
mySheet[1,"Description"]="Another description"
#NOTE: this syntax preserves types and factor levels, and adds new rows if necessary. mySheet$Name="Entire Forest" does not.
saveDatasheet(myProject,mySheet,name=sheetName)

datasheet(myProject,name=sheetName,empty=T,optional=F) #returns only truly required columns
datasheet(myProject,name=sheetName,empty=F,optional=F) #returns required columns and columns with data
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

# Now lookups are loaded we can set StateClass
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"StateLabelXID"] ="hi" #Invalid value for a lookup column
mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
saveDatasheet(myProject,mySheet,name=sheetName)
# NOTE: special knowledge needed to construct Name here. 

str(datasheet(myProject,sheetName))
str(datasheet(myProject,sheetName,lookupsAsFactors = F)) #lookups are not returned as factors
datasheet(myProject,sheetName,includeKey=T) #include primary key for datasheet
datasheet(myProject,sheetName,optional=T) #include empty optional columns

#***********************************
# Transitions
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
# Build Scenario That Contains Shared Parameters
#*************************************
myScenario = scenario(myProject,scenario="Dependency Scenario")
subset(datasheet(myScenario),scope=="scenario")$name #see scenario scope datasheets

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
sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
levels(mySheet$StateClassIDSource)
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1"))
mySheet=addRow(mySheet,list(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1"))
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1"))
mySheet
saveDatasheet(myScenario,mySheet,name=sheetName)
# mySheet = datasheet(myScenario,name=sheetName,optional=T); str(mySheet) #check what happened
# addRow() checks validity of column names and factor levels.
# addRow() fills missing values using factor levels where possible.

#*************************
# Probabilistic transitions
sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.01))
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Harvest",Probability=1,AgeMin=40))
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.002))
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=10))
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.005))
mySheet=addRow(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=20))
mySheet
saveDatasheet(myScenario,mySheet,name=sheetName)
#mySheet = datasheet(myScenario,name=sheetName,optional=T); mySheet #check what happened

#********************
#Initial conditions
sheetName = "STSim_InitialConditionsNonSpatial"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
mySheet[1,"TotalAmount"]=1000
mySheet[1,"NumCells"]=1000
saveDatasheet(myScenario,mySheet,name=sheetName)

sheetName = "STSim_InitialConditionsNonSpatialDistribution"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
mySheet=addRow(mySheet,data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20))
mySheet=addRow(mySheet,data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40))
mySheet=addRow(mySheet,data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40))
mySheet
saveDatasheet(myScenario,mySheet,name=sheetName)

#*************************************
# Add No Harvest Scenario
#*************************************
myScenario = scenario(myProject,scenario="No Harvest")

#set and remove dependency
dependency(myScenario,dependency="Dependency Scenario") #set dependency
dependency(myScenario) #now there is a dependency 
dependency(myScenario,dependency="Dependency Scenario",remove=T,force=T)
dependency(myScenario) #dependency has been removed
dependency(myScenario,dependency="Dependency Scenario") #set dependency

#NOTE: dependency datasheets are not found in myScenario.
sheetName = "STSim_InitialConditionsNonSpatialDistribution"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=F)
str(mySheet) #note this datasheet is a dependency - it cannot be found in my scenario

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
myScenario = scenario(myProject,scenario="Harvest",sourceScenario="No Harvest")
# Copies "No Harvest" scenario to new "Harvest" scenario - including dependencies.
dependency(myScenario)

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
myResults = run(myProject,scenario=c("Harvest","No Harvest"),jobs=4)

scnList = scenario(myProject,scenario=c("Harvest","No Harvest"))
otherResults = run(scnList,jobs=4,summary=T)
str(otherResults)
# By default, returns a named list of result Scenario objects.
# If summary=T (slightly faster), returns result scenario summary info instead of objects
# NOTE: jobs is passed through to SyncroSim which handles multithreading.

scenario(myProject)
a=runLog(myResults[[1]]) #displays and returns a multiline string
a #here is the multiline string. not pretty.
writeLines(a) #use writelines to display the linebreaks

parentId(myResults[[1]])

#********************************
# See results
#******************************
outStates = datasheet(myResults,name="STSim_OutputStratumState")
str(outStates)
# NOTE: For outputs, if lookupsAsFactors=F id's will be returned, rather than text labels.
# Output table lookups are IDs in the database, rather than labels - not true for input tables.
#
# NOTE: can query multiple projects or scenarios - see ?datasheet for details.
#
# NOTE: We can also query the database more precisely to avoid pulling unecessary information.
# There are >400,000 records in this small example.
sheetName = "STSim_OutputStratumState"
names(datasheet(myResults,name=sheetName,lookupsAsFactors=F,empty=T,optional=T)) #Get column names without getting data
unique(outStates$Iteration)
mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","StateLabelXID"),aggregate=c("Amount"),where=list(Timestep=c(0,1,2),Iteration=c(3,4)))
mySQL # A list of SELECT, WHERE and GROUP BY SQL statements passed to SQLite.
outStatesAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
str(outStatesAllAges) #Much faster: fewer lookups and fewer records.

sheetName = "STSim_OutputStratumTransition"
names(datasheet(myResults,name=sheetName,lookupsAsFactors=F,empty=T)) #Get column names without getting any data
mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","TransitionGroupID"),aggregate=c("Amount"))
outTransitionsAllAges = datasheet(myResults,name=sheetName,optional=F,empty=T)
str(outTransitionsAllAges)
outTransitionsAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL,optional=T) #note optional argument is ignored if sqlStatements$select specifies columns to select
str(outTransitionsAllAges)

# NOTE: In the following example we use existing R tools (ggplot2/plyr) to visualize the output.
# install.packages("ggplot2");install.packages("plyr")
library(ggplot2);library(plyr)

#Example visualization - mean and 95% confidence bands for area in each state over time.
outStatesSummary = ddply(outStatesAllAges,.(Timestep,StateLabelXID,ParentName),summarize,amount=mean(Amount),upperAmount=quantile(Amount,0.975),lowerAmount=quantile(Amount,0.025))
base = ggplot(outStatesSummary,aes(x=Timestep,y=amount,ymax=upperAmount,ymin=lowerAmount))+geom_line()+geom_ribbon(alpha=0.1)
base=base+facet_grid(StateLabelXID~ParentName)+ theme_bw()
base=base+ylab("area (acres)")
print(base)

#Example visualization - mean and 95% confidence bands for transitions over time.
outTransitionsSummary = ddply(outTransitionsAllAges,.(Timestep,TransitionGroupID,ParentName),summarize,amount=mean(Amount),upperAmount=quantile(Amount,0.975),lowerAmount=quantile(Amount,0.025))
base = ggplot(outTransitionsSummary,aes(x=Timestep,y=amount,ymax=upperAmount,ymin=lowerAmount))+geom_line()+geom_ribbon(alpha=0.1)
base=base+facet_grid(TransitionGroupID~ParentName,scales="free_y")+ theme_bw()
base=base+ylab("area (acres)")
print(base)
