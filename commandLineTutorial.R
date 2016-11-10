# **********************************************************
# commandLineTutorialRMockup.R
# Mock R code following the steps in Leo's PowerShell script.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.10.13
# **********************************************************

# devtools::document();devtools::load_all()

# The working directory path and the name of the library you will create
workingDir = "C:/Temp"

#*************************************
# Create the project definition
myLibrary = ssimLibrary(model="stsim",name=paste0(workingDir,"/ST-Sim-Command-Line.ssim"))
myProject = project(myLibrary,name="ST-Sim Demonstration")
sheetNames = datasheets(myProject,names=T)

#***********************************
# Cover types and state classes
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
str(mySheet)
sheetName = "STSim_TransitionGroup"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
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
sheetNames$name[sheetNames$dataScope=="project"]
sheetName = "STSim_AgeType"; mySheet = datasheet(myProject,name=sheetName,sheetNames=sheetNames)
str(mySheet)
mySheet[1,"Frequency"] = 1
mySheet[1,"MaximumAge"] = 100
loadDatasheets(myProject,mySheet,name=sheetName,sheetNames=sheetNames)

#*************************************
# Add No Harvest Scenario
#*************************************
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
sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames,empty=T)
str(mySheet)
levels(mySheet$StateClassIDSource)
addRow(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1")
addRow(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1")
addRow(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1")
#addRow fills in missing values using factor levels where possible.
#addRow also complains if factor values are not valid
mySheet
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)
#mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames)
#str(mySheet)

#*************************
# Probabilistic transitions
sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames,empty=T)
addRow(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.01)
addRow(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Harvest",Probability=1,AgeMin=40)
addRow(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.002)
addRow(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=10)
addRow(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All",
                           TransitionTypeID="Fire",Probability=0.005)
addRow(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All",
                           TransitionTypeID="Succession",Probability=0.1,AgeMin=20)
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)
#mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames)
#mySheet

#********************
#Initial conditions
# devtools::document();devtools::load_all()
sheetName = "STSim_InitialConditionsNonSpatial"; mySheet = datasheet(myScenario,name=sheetName,optional=F,sheetNames=sheetNames,empty=T)
mySheet[1,"TotalAmount"]=1000
mySheet[1,"NumCells"]=1000
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)

sheetName = "STSim_InitialConditionsNonSpatialDistribution"; mySheet = datasheet(myScenario,name=sheetName,optional=T,sheetNames=sheetNames,empty=T)
addRow(mySheet)=data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20)
addRow(mySheet)=data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40)
addRow(mySheet)=data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40)
mySheet
loadDatasheets(myScenario,mySheet,name=sheetName,sheetNames=sheetNames)

#*************************************
#Add Harvest Scenario
#*************************************
myScenario = scenario(myProject,name="Harvest",sourceScenario="No Harvest")
#copies "No Harvest" scenario to new "Harvest" scenario

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
