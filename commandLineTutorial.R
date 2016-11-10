# **********************************************************
# commandLineTutorialRMockup.R
# Mock R code following the steps in Leo's PowerShell script.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.10.13
# **********************************************************

# **********************************************************
# Define the project
# **********************************************************

# devtools::document();devtools::load_all()

# The working directory path and the name of the library you will create
workingDir = "C:/Temp"
#setwd("C:/gitprojects/rsyncrosim")

#*************************************
# Create the project definition
myLibrary = ssimLibrary(model="stsim",name=paste0(workingDir,"/ST-Sim-Command-Line.ssim"))

myProject = project(myLibrary,name="ST-Sim Demonstration")

sheetNames = datasheets(myProject,names=T)
#***********************************
# Cover types and state classes
# devtools::document();devtools::load_all()
sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName)
mySheet[1,"Name"]="Entire Forest"
loadDatasheets(myProject,mySheet,name=sheetName)
#NOTE: datasheet() retrieval is slow because it requires at least 2 system calls (export and list columns).
#Currently, another system call is required to determine scope in both datasheet() and loadDatasheets().
#That system call is avoided if the user provides sheetNames.
#Do we want to consider options for speeding the process?
#Another system call is required for each dependency.

# Warn if dependencies are not loaded, and return a factor with 0 levels
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1,"StateLabelYID"]="All" #A more cryptic warning because the factor has no levels.

sheetName = "STSim_StateLabelY"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
mySheet[1,"Name"]="All"
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)

sheetName = "STSim_StateLabelX"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
mySheet[1:3,"Name"]=c('Coniferous','Deciduous','Mixed')
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)

# Now dependencies are loaded we can set StateClass
sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1,"StateLabelXID"] ="hi" #Invalid value for a column with dependency
mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)
#sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
#str(mySheet)

# DISCUSS: dependencies. Is this enough? If not, what else is needed?
# DISCUSS: special knowledge needed to construct Name here?
# LOW PRIORITY: Defaults for primary and secondary strata?

#***********************************
# Transitions
sheetNames$name[sheetNames$dataScope=="project"]
sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)
loadDatasheets(myProject,mySheet,name="STSim_TransitionType",sheetNames=sheetNames)

sheetName = "STSim_TransitionTypeGroup"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1:3,"TransitionTypeID"]=levels(mySheet$TransitionTypeID)
mySheet[1:3,"TransitionGroupID"]=levels(mySheet$TransitionGroupID)
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)

#****************
# Age type
# devtools::document();devtools::load_all()
sheetNames$name[sheetNames$dataScope=="project"]
sheetName = "STSim_AgeType"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1,"Frequency"] = 1
mySheet[1,"MaximumAge"] = 100
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)

#*************************************
# Add No Harvest Scenario
#*************************************
scenarioName="No Harvest"
myScenario = scenario(myProject,name="No Harvest")

sheetNames = datasheets(myScenario,names=T)
sheetNames$name[sheetNames$dataScope=="scenario"]

#**************
# Run control
sheetName = "STSim_RunControl"; mySheet = datasheet(myScenario,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1,"MinimumIteration"] = 1
mySheet[1,"MaximumIteration"] = 40
mySheet[1,"MaximumTimestep"] = 0
mySheet[1,"MaximumTimestep"] = 50
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)

#**************************
# Deterministic transitions
# devtools::document();devtools::load_all()
sheetNames$name[sheetNames$dataScope=="scenario"]
sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames)
# NOTE: dependencies are time consuming
str(mySheet)
mySheet = allCombos(mySheet)
str(mySheet)
mySheet=setRows(mySheet,
  conditions=list(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All"),
  values=list(AgeMin=21,Location="C1"))
mySheet=setRows(mySheet,
  conditions=list(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All"),
  values=list(Location="A1"))
mySheet=setRows(mySheet,
  conditions=list(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All"),
  values=list(AgeMin=11,Location="B1"))
mySheet=subset(mySheet,!is.na(Location))
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)
#mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames)
#str(mySheet)

sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames)
str(mySheet)
mySheet = allCombos(mySheet)
str(mySheet)
mySheet=setRows(mySheet,
  conditions=list(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",TransitionTypeID="Fire"),
  values=list(Probability=0.01))
mySheet=setRows(mySheet,
  conditions=list(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",TransitionTypeID="Harvest"),
  values=list(Probability=1,AgeMin=40))
mySheet=setRows(mySheet,
  conditions=list(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",TransitionTypeID="Harvest"),
  values=list(Probability=1,AgeMin=40))

mySheet=subset(mySheet,!is.na(Probability))
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)
#mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames)
#str(mySheet)


detTransitions = GetOptions(myProject[[scenarioName]],dataSheet="Deterministic Transitions")
#GetOptions returns a data frame with all possible stratum and state class sources and destinations.
#If the number of possible options is too large, warn the user and return a subset, with possible values for each column in the factor levels
#The user can subset and modify the transition table as appropriate.
#SetValues modifies values on one or more rows of a data frame. Values are assigned to all rows that meet the conditions.
#Users can also load or generate their own data frame.
detTransitions=SetValues(detTransitions,conditions=list(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All"),
                         values=list(AgeMin=21,Location="C1"))
detTransitions$Location[(StateSource=="Deciduous:All")&(StateDest=="Deciduous:All")]="A1"
detTransitions=SetValues(detTransitions,conditions=list(StateSource="Mixed:All",StateDest="Mixed:All"),
                         values=list(AgeMin=11,Location="B1"))
myProject = SetDataSheet(myProject[[scenarioName]],dataSheet="Deterministic Transitions",values=detTransitions)
#Rows without probabilities will be automatically dropped from the table.

#*************************
#Probabilistic transitions
probTransitions = GetOptions(myProject[[scenarioName]],dataSheet="Probabilistic Transitions")
probTransitions$Probability[(StateSource=="Coniferous:All")&(StateDest=="Deciduous:All")&(Type="Fire")]=0.01
probTransitions=SetValues(probTransitions,conditions=list(StateSource="Coniferous:All",StateDest="Deciduous:All",Type="Harvest"),
                          values=list(AgeMin=40,Probability=1))
probTransitions$Probability[(StateSource=="Deciduous:All")&(StateDest=="Deciduous:All")&Type="Fire"]=0.002
probTransitions=SetValues(probTransitions,conditions=list(StateSource="Decidious:All",StateDest="Mixed:All",Type="Succession"),
                          values=list(AgeMin=10,Probability=0.1))
probTransitions$Probability[(StateSource=="Mixed:All")&(StateDest=="Deciduous:All")&Type="Fire"]=0.005
probTransitions=SetValues(probTransitions,conditions=list(StateSource="Mixed:All",StateDest="Coniferous:All",Type="Succession"),
                          values=list(AgeMin=20,Probability=0.1))
myProject = SetDataSheet(myProject[[scenarioName]],dataSheet="Probabilistic Transitions",values=probTransitions)
#The user can subset, modify or overwrite the options table in the usual manner.

#********************
#Initial conditions
inits = GetOptions(myProject[[scenarioName]],dataSheet="Initial Conditions Non Spatial")
inits= AddRow(TotalAmount=1000,NumCells=1000)
myProject = SetDataSheet(myProject[[scenarioName]],dataSheet="Initial Conditions Non Spatial",values=inits)

nsInitialConditions = GetOptions(myProject[[scenarioName]],dataSheet="Initial Cond. Distribution")
nsInitialConditions=SetValues(nsInitialConditions,conditions=list(State="Coniferous:All"),values=list(AgeMin=20,AgeMax=100,RelativeAmount=20))
nsInitialConditions=SetValues(nsInitialConditions,conditions=list(State="Decidious:All"),values=list(AgeMax=10,RelativeAmount=40))
nsInitialConditions=SetValues(nsInitialConditions,conditions=list(State="Mixed:All"),values=list(AgeMin=11,AgeMax=20,RelativeAmount=40))
myProject = SetDataSheet(myProject[[scenarioName]],dataSheet="Initial Cond. Distribution",values=nsInitialConditions)

#*************************************
#Add Harvest Scenario
#*************************************
scenarioName="Harvest"
myProject = AddScenario(myProject,scenarioName,sourceScenario="No Harvest")
#Adds a copy of the source scenario to the project.

#******************
#Transition targets
transitionTargets = GetOptions(myProject[[scenarioName]],dataSheet="Transition Targets")
transitionTargets$Amount[transitionGroupID=='Harvest'] = 20
myProject = SetDataSheet(myProject[[scenarioName]],dataSheet="Transition Targets",values=transitionTargets)

#********************************
#Run scenarios
#******************************
ssResults = SyncroSimRun(myProject,scenarios=c("No Harvest","Harvest"),cores=2)
#SyncroSimRun does all the work, and returns an object containing the project definition and info necessary for querying the output.
#To do:
# - Project revisions: Safe modification of existing libraries?
# - Parallel processing

#********************
#Other useful functions
Status(ssResults)
#status of each scenario in the project

#*****************
#To do:
#*********************
# - break points
# - output

######################
#Misc notes, thoughts, extra code bits - ignore.
