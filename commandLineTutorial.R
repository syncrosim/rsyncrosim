# **********************************************************
# commandLineTutorialRMockup.R
# Mock R code following the steps in Leo's PowerShell script.
# The R package rstan may be a useful example: http://mc-stan.org/interfaces/rstan
# Stan is software for Bayesian statistical modelling, written in C++.
# Bayesian methods are computationally intensive, so users typically want to run many mcmc chains in parallel.
# All of the complexity is wrapped in a single call to stan()
# Borrowing from rstan, the approach here is:
#  1) User does a bunch of work to set up the model. In our case, it will be a project definition object,
#     containing all the datafeeds and datasheets need by syncrosim.
#  2) User calls a function (SyncroSimRun) that does a ton of work: creating (or modifying) the library,
#     project, scenarios, etc; running the simulations (in parallel if needed); returning an object containing
#     the project definition and info required to access the output.
#  3) User does a bunch of stuff with the output (not done yet). We provide methods for querying the output.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.10.13
# **********************************************************

# **********************************************************
# Define the project
# **********************************************************

# The working directory path and the name of the library you will create
workingDir = "C:/Users/Josie Hughes/Documents/ApexLocal/BashShellTutorial/Temp"
libName = "ST-Sim-Command-Line.ssim"
# The name of the project you want to create
prjName = "ST-Sim Demonstration"
primaryModule = "STSim"

#*************************************
#Create the project definition
myProject = SyncroSimNewProjectDefinition(workingDir,libName,prjName,primaryModule)
#Method returns a SyncroSimProjectDefinition object containing lists of library and project scope datasheets.
#Datasheet lists are populated from the appropriate model.config xml files.
#Datasheet objects contain
#  descriptors: name, displayName,dataScope, etc.
#  columnDescriptions: name, dataType, etc.
#  validations: from the xml file
#  options: an example data frame with expected column names. In case of constraints on allowed values (i.e. formula1=), the options data frame will include a row for each allowable combination (e.g. every possible transition, unless the number of possible combinations is excessive). Otherwise there will be no rows in the options data frame.
#  defaults: we may wish to supply defaults for some data sheets.
#  values: blank until a data frame is supplied by the user.
#Fail if there is already a project of that name in the library.
#Alternatively, load an existing project from disk:
#  myProject = SyncroSimLoadProjectDefinition(workingDir,libName,prjName)

#Query the status of the project definition. What info is required?
Status(myProject)
#Returns a data frame containing info on each data sheet: required or not? has defaults? has values?

#***********************************
#Cover types and state classes
coverTypes = GetOptions(myProject,dataSheet="State Label X") #returns a data frame with no rows - there are no constraints on this yet
coverTypes$Name=c('Coniferous','Deciduous','Mixed')
myProject = SetDataSheet(myProject,dataSheet="State Label X",values=coverTypes) #The "State Label X" data sheet now contains values.
#SetDataSheet will do some checking to ensure that the values are valid.

stateClass = GetOptions(myProject,dataSheet="State Class",useDefaults=T)
#Assume that we have a single default name "All" for "State Label Y".
#GetOptions will return a data frame containing all possible combinations of "State Label X" and "State Label Y".
#If either of these datasheets did not yet contain defaults or values, GetOptions would prompt the user to enter the missing info before continuing.
myProject = SetDataSheet(myProject,dataSheet="State Class",values=stateClass)
#The user could choose to modify the stateClass data frame before calling SetDataSheet

#Use defaults for primary and secondary strata

#***********************************
#Transitions
transitionTypes = GetOptions(myProject,dataSheet="Transition Type")
transitionTypes$Name = c("Fire","Harvest","Succession")
myProject = SetDataSheet(myProject,dataSheet="Transition Type",values=transitionTypes)
myProject = SetDataSheet(myProject,dataSheet="Transition Group",values=transitionTypes)

typeGroups = GetOptions(myProject, dataSheet="Transition Types By Group")
typeGroups = subset(typeGroups,TransitionTypeID==TransitionGroupID)
myProject = SetDataSheet(myProject, dataSheet="Transition Types By Group",values=typeGroups)
#In the this case all possible combinations would not be ok, but this would not be clear from the config file.
#Worry about stuff like this once the basic framework is in place?

#****************
#Age type
ageType=GetOptions(myProject,dataSheet="Age Types") #returns a data frame with no rows - there are no constraints on values
ageType = AddRow(Frequency=1,MaximumAge=100)
ageType=SetDataSheet(myProject,dataSheet="Age Types")

#*************************************
#Add No Harvest Scenario
#*************************************
scenarioName="No Harvest"
myProject = AddScenario(myProject,scenarioName)
#Method adds an SyncroSimScenario object to the project definition.
#SyncroSimScenario object contains a list of scenario level datasheets.
#Fail if there is already a scenario of that name in the project.
#Fail if necessary project and library scope datasheets are not complete.

#Query the status of the scenario definition. What info is required?
Status(myProject[[scenarioName]])
#Returns a data frame containing info on each data sheet: required or not? has defaults? has values?

#**************
#Run control
runControl = getOptions(myProject[[scenarioName]],dataSheet="Run Control")
runControl = AddRow(MinimumIteration=1,MaximumIteration=40,MinimumTimestep=0,MaximumTimestep=50)
myProject = SetDataSheet(myProject,scenario=scenarioName,dataSheet="Run Control",values=runControl)

#**************************
#Deterministic transitions
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
SetValues<-function(x,conditions,values){
  x$IsIn = T
  for(i in seq(length(conditions))){
    x$IsIn[x[[names(conditions[i])]]!=conditions[i]]=F
  }

  for(i in seq(length(values))){
    x[[names(values)[i]]][x$IsIn]=values[i]
  }
  x$IsIn=NULL
  return(x)
}
