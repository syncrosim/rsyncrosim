# **********************************************************
# commandLineTutorial.R
# Following the steps in Leo's PowerShell script using rsyncrosim.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.10
# **********************************************************
# devtools::document();devtools::load_all()
?datasheet
# The working directory path and the name of the library you will create
workingDir = "C:/Temp"

#*************************************
# Create the project definition
myLibrary = ssimLibrary(model="stsim",name=paste0(workingDir,"/ST-Sim-Command-Line.ssim"))
myProject = project(myLibrary,name="ST-Sim Demonstration")
datasheets(myProject,names=T)

#***********************************
# Cover types and state classes
#RESUME HERE - understand package dependencies.
sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=F)
mySheet[1,"Name"]="Entire Forest"
loadDatasheets(myProject,mySheet,name=sheetName)
#NOTE: Default datasheet() retrieval requires a database query and at least 1 console call (empty=F, stringsAsFactors=T)
#A console call is required for each dependency, so the default datasheet() can be quite slow.
#Setting empty=T eliminates the database query.
#Do we want to consider other options for speeding the process?

# Warn if dependencies are not loaded, and return a factor with 0 levels
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
#sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName)
#str(mySheet)

# DISCUSS: dependencies. Is this enough? If not, what else is needed?
# DISCUSS: special knowledge needed to construct Name here?
# LOW PRIORITY: Defaults for primary and secondary strata?

#***********************************
# Transitions
sheetNames$name[sheetNames$dataScope=="project"]
str(mySheet)
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
sheetNames$name[sheetNames$dataScope=="project"]
sheetName = "STSim_AgeType"; mySheet = datasheet(myProject,name=sheetName,empty=T)
str(mySheet)
mySheet[1,"Frequency"] = 1
mySheet[1,"MaximumAge"] = 100
loadDatasheets(myProject,mySheet,name=sheetName)

#*************************************
# Add No Harvest Scenario
#*************************************
myScenario = scenario(myProject,name="No Harvest")
sheetNames = datasheets(myScenario,names=T)
sheetNames$name[sheetNames$dataScope=="scenario"]

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
sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
str(mySheet)
levels(mySheet$StateClassIDSource)
addRow(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1")
addRow(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1")
addRow(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1")
#addRow fills in missing values using factor levels where possible.
#addRow also complains if factor values are not valid
mySheet
loadDatasheets(myScenario,mySheet,name=sheetName)
#mySheet = datasheet(myScenario,name=sheetName,optional=T)
#str(mySheet)

#*************************
# Probabilistic transitions
sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
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
loadDatasheets(myScenario,mySheet,name=sheetName)
#mySheet = datasheet(myScenario,name=sheetName,optional=T)
#mySheet

#********************
#Initial conditions
# devtools::document();devtools::load_all()
sheetName = "STSim_InitialConditionsNonSpatial"; mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
mySheet[1,"TotalAmount"]=1000
mySheet[1,"NumCells"]=1000
loadDatasheets(myScenario,mySheet,name=sheetName)

sheetName = "STSim_InitialConditionsNonSpatialDistribution"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
addRow(mySheet)=data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20)
addRow(mySheet)=data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40)
addRow(mySheet)=data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40)
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
deleteScenarios(myProject,"Harvest",force=T)
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

myResults = run(myProject,scenario=c("Harvest","No Harvest"))
#By default, returns a named list of result Scenario objects.
#If onlyIds = TRUE (faster), returns result scenario ids instead of objects

#TO DO: multiple threads
scenarios(myProject,names=T)
harvestResult = myResults[["Harvest"]]

#********************************
# See results
#******************************
# devtools::document();devtools::load_all()
#myResults=scenarios(myProject,results=T);names(myResults) = c("Harvest","No Harvest")

# When given a list of Scenario objects, datasheet() binds over scenarios.
outStates = datasheet(myResults,name="STSim_OutputStratumState",dependsAsFactors=F)
unique(outStates$scenario)
outTransitions = datasheet(myResults,name="STSim_OutputStratumTransition",dependsAsFactors=F)

# DISCUSS: to what extent (if any) do we want to reimplement ggplot2/plyr functionality for summarizing and visualizing output?
# install.packages("ggplot2");install.packages("plyr")
library(ggplot2);library(plyr)

#Example visualization - mean and 95% confidence bands for area in each state over time.
outStatesAllAges = ddply(outStates,.(Timestep,StateLabelXID,scenario,Iteration),summarize,Amount=sum(Amount)) #summarize over ages
outStatesSummary = ddply(outStatesAllAges,.(Timestep,StateLabelXID,scenario),summarize,amount=mean(Amount),upperAmount=quantile(Amount,0.975),lowerAmount=quantile(Amount,0.025))
base = ggplot(outStatesSummary,aes(x=Timestep,y=amount,ymax=upperAmount,ymin=lowerAmount))+geom_line()+geom_ribbon(alpha=0.1)
base=base+facet_grid(StateLabelXID~scenario)+ theme_bw()
base=base+ylab("area (acres)")
print(base)

#Example visualization - mean and 95% confidence bands for transitions over time.
outTransitionsAllAges = ddply(outTransitions,.(Timestep,TransitionGroupID,scenario,Iteration),summarize,Amount=sum(Amount)) #summarize over ages
outTransitionsSummary = ddply(outTransitionsAllAges,.(Timestep,TransitionGroupID,scenario),summarize,amount=mean(Amount),upperAmount=quantile(Amount,0.975),lowerAmount=quantile(Amount,0.025))
base = ggplot(outTransitionsSummary,aes(x=Timestep,y=amount,ymax=upperAmount,ymin=lowerAmount))+geom_line()+geom_ribbon(alpha=0.1)
base=base+facet_grid(TransitionGroupID~scenario,scales="free_y")+ theme_bw()
base=base+ylab("area (acres)")
print(base)
