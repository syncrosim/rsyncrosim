# ********************************************
# rsyncrosim-demo.R   Demo using rsyncrosim package
# **********************************************************

# *************************************************************
# File/Package Setup
# *************************************************************

# *** Install latest version of package (if necessary)
# install.packages("C:/Temp/rsyncrosim_0.1.0.tar.gz",repos=NULL)    # Download development version of rsyncrosim
# ***

#library(rsyncrosim)
library(raster)

# Setup files and folders
# TODO: We need a way to include the input data GeoTIFF files
# TODO: is there a way that we can make the working folder something generic that will work for everyone?
workingFolder = "C:/Users/leona/Documents/SyncroSim/rsyncrosim"
programFolder = "C:/Program Files/SyncroSim"
moduleFolder = "C:/Temp"
moduleFilename1 = 'stsim-3-1-1-x64.ssimpkg'
moduleFilename2 = 'stsim-stockflow-3-1-1-x64.ssimpkg'

# Raster initial conditions filenames
stratumTif = paste(workingFolder, "initial-stratum.tif", sep="/")
sclassTif = paste(workingFolder, "initial-sclass.tif", sep="/")
ageTif = paste(workingFolder, "initial-age.tif", sep="/")

setwd(workingFolder)
getwd()

# *************************************************************
# Start Session & Create Library
# *************************************************************

# Start a SyncroSim session
mySession = session(programFolder)

# Check stsim and stim-stockflow modules are loaded in the session
module(mySession)

# *** Reload modules if they are missing or out of date  (if necessary)
# moduleFilepaths = c(paste(moduleFolder,moduleFilename1,sep="/"), paste(moduleFolder,moduleFilename1,sep="/"))
# addModule(moduleFilepaths, mySession)
# ***

# Create a new library (or load it if it already exists)
myLibrary = ssimLibrary(name="Demonstration Library", model="stsim", session=mySession)

# Check if stock flow add-on is enabled for the library
addon(myLibrary)

# *** Enable stock flow add-on (if necessary)
# enableAddon(myLibrary, 'stsim-stockflow')
# addon(myLibrary)
# ***

# Display internal names of the library's datasheets
librarySheetNames = datasheet(myLibrary, summary=T)
librarySheetNames

# Modify library datasheet to include input spatial files in backups
sheetData = datasheet(myLibrary, name="SSim_Backup")                       # Get the current backup settings for the library
sheetData = addRow(sheetData, data.frame(IncludeInput=T, IncludeOutput=F)) # Add a new row to this dataframe
saveDatasheet(myLibrary, data=sheetData, name="SSim_Backup")               # Save the new dataframe back to the library

# Backup the library before making changes
backup(myLibrary)

# *************************************************************
# Create Project
# *************************************************************

# *** For debugging - delete project first
# delete(myProject, force=T)
# ***

# Create a new SyncroSim project (or open it if it already exists)
myProject = project(myLibrary, project="Simple forest landscape")

# Display internal names of the project's datasheets
projectSheetNames=datasheet(myProject, summary=T, optional=F)

# Edit the Project Definitions:

# Terminology
sheetData = datasheet(myProject, "STSim_Terminology")
sheetData
str(sheetData)
sheetData$AmountUnits[1] = "Hectares"
sheetData$StateLabelX[1] = "Forest Type"
saveDatasheet(myProject, sheetData, "STSim_Terminology")

# Stratum
sheetData = datasheet(myProject, "STSim_Stratum", empty=T)   # Returns empty dataframe with only required column(s)
sheetData = addRow(sheetData, "Entire Forest")
saveDatasheet(myProject, sheetData, "STSim_Stratum", force=T)
datasheet(myProject, "STSim_Stratum", optional=T)   # Returns entire dataframe, including optional columns

# First State Class Label (i.e. Forest Types)
forestTypes = c("Coniferous", "Deciduous", "Mixed")
saveDatasheet(myProject, data.frame(Name=forestTypes), "STSim_StateLabelX", force=T)

# Second State Label (not used)
saveDatasheet(myProject, data.frame(Name=c("All")), "STSim_StateLabelY", force=T)

# Transition Types
transitionTypes = data.frame(Name=c("Fire", "Harvest", "Succession"), ID=c(1,2,3))
saveDatasheet(myProject, transitionTypes, "STSim_TransitionType", force=T)

# Transition Groups - note that all Types still need to be added as Groups when using the command line
transitionGroups = data.frame(Name=c("Fire", "Harvest", "Succession"))
saveDatasheet(myProject, transitionGroups, "STSim_TransitionGroup", force=T)

# Transition Types by Groups
transitionTypesGroups = data.frame(TransitionTypeID=transitionTypes$Name, TransitionGroupID=transitionGroups$Name)
saveDatasheet(myProject, transitionTypesGroups, "STSim_TransitionTypeGroup", force=T)

# State Classes
stateClasses = data.frame(Name=forestTypes)
stateClasses$StateLabelXID = stateClasses$Name
stateClasses$StateLabelYID = "All"
stateClasses$ID = c(1,2,3)
saveDatasheet(myProject, stateClasses, "STSim_StateClass", force=T)

# Ages
ageFrequency = 1
ageMax = 101
ageGroups = c(20,40,60,80,100)
saveDatasheet(myProject, data.frame(Frequency=ageFrequency, MaximumAge=ageMax), "STSim_AgeType", force=T)
saveDatasheet(myProject, data.frame(MaximumAge=ageGroups), "STSim_AgeGroup", force=T)

# *************************************************************
# Create Scenarios
# *************************************************************

# *** For debugging - delete scenarios first
# delete(myProject, scenario=c("No Harvest", "Harvest"), force=T)
# ***

# Create/open a SyncroSim "No Harvest" scenario
myScenario = scenario(myProject, "No Harvest")

# Display the internal names of all the scenario datasheets
scenarioSheetNames=subset(datasheet(myScenario, summary=T), scope=="scenario")
scenarioSheetNames

# Edit the scenario datasheets:

# Run Control
sheetName = "STSim_RunControl"
sheetData = data.frame(MaximumIteration=7, MinimumTimestep=0, MaximumTimestep=50, isSpatial=T)
saveDatasheet(myScenario, sheetData, sheetName)

# States
sheetName = "STSim_DeterministicTransition"
sheetData = datasheet(myScenario, sheetName , optional=T, empty=T)
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1"))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1"))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1"))
saveDatasheet(myScenario, sheetData, sheetName)

# Probabilistic Transitions
sheetName = "STSim_Transition"
sheetData = datasheet(myScenario, sheetName , optional=T, empty=T)
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All", TransitionTypeID="Fire",Probability=0.01))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All", TransitionTypeID="Harvest",Probability=1,AgeMin=40))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All", TransitionTypeID="Fire",Probability=0.002))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All", TransitionTypeID="Succession",Probability=0.1,AgeMin=10))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All", TransitionTypeID="Fire",Probability=0.005))
sheetData = addRow(sheetData, data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All", TransitionTypeID="Succession",Probability=0.1,AgeMin=20))
saveDatasheet(myScenario, sheetData, sheetName)

# Check rasters (for initial conditions)
rStratum=raster(stratumTif)
rSclass=raster(sclassTif)
rAge=raster(ageTif)
plot(rStratum)
plot(rSclass)
plot(rAge)

# Set Spatial initial conditions
sheetName = "STSim_InitialConditionsSpatial"
sheetData = data.frame(StratumFileName=stratumTif, StateClassFileName=sclassTif, AgeFileName=ageTif)
saveDatasheet(myScenario, sheetData, sheetName)
datasheet(myScenario, sheetName)

# Check rasters correcly loaded
rStratumTest = datasheetRaster(myScenario, sheetName, "StratumFileName")
rSclassTest = datasheetRaster(myScenario, sheetName, "StateClassFileName")
rAgeTest = datasheetRaster(myScenario, sheetName, "AgeFileName")
plot(rStratumTest)
plot(rSclassTest)
plot(rAgeTest)

# Transition targets - set harvest to 0
saveDatasheet(myScenario, data.frame(TransitionGroupID="Harvest", Amount=0), "STSim_TransitionTarget")

# Output options
datasheet(myScenario, "STSim_OutputOptions")
sheetData = data.frame(SummaryOutputSC=T, SummaryOutputSCTimesteps=1,
                       SummaryOutputTR=T, SummaryOutputTRTimesteps=1,
                       RasterOutputSC=T, RasterOutputSCTimesteps=1,
                       RasterOutputTR=T, RasterOutputTRTimesteps=1,
                       RasterOutputAge=T, RasterOutputAgeTimesteps=1)
saveDatasheet(myScenario, sheetData, "STSim_OutputOptions")

# Create a second "Harvest" scenario with a harvest level of 20 acres/timestep
myScenarioHarvest = scenario(myProject, scenario="Harvest", sourceScenario=myScenario)
saveDatasheet(myScenarioHarvest, data.frame(TransitionGroupID="Harvest", Amount=20), "STSim_TransitionTarget")

# Show all the harvest levels
datasheet(myProject, scenario = c("Harvest", "No Harvest"), name= "STSim_TransitionTarget")

# *************************************************************
# Run Scenarios & Get Results
# *************************************************************

# Run 2 scenarios
resultSummary = run(myProject, scenario=c("Harvest", "No Harvest"), jobs=7, summary=T)

# Get the scenario IDs of the 2 result scenarios
resultIDNoHarvest = subset(resultSummary, parentID==scenarioId(myScenario))$scenarioId
resultIDHarvest = subset(resultSummary, parentID==scenarioId(myScenarioHarvest))$scenarioId

# View state class tabular output (for both scenarios combined)
outputStratumState = datasheet(myProject, scenario=c(resultIDNoHarvest,resultIDHarvest), name="STSim_OutputStratumState")

# View state class raster output (for the Harvest scenario only)
# TODO: it would be good if these plots in their legends use the actual factor names rather than ID values for state class and transition group.
myRastersTimestep5 = datasheetRaster(myProject, scenario=resultIDHarvest, "STSim_OutputSpatialState", timestep=5)
myRastersTimestep5
plot(myRastersTimestep5)

myRasterTransitionTimestep5 = datasheetRaster(myProject, scenario=resultIDHarvest, "STSim_OutputSpatialTransition", timestep=5, iteration = 1)
myRasterTransitionTimestep5
plot(myRasterTransitionTimestep5)




