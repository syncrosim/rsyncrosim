# ********************************************
# rsyncrosim-vignette.R   Base for rsyncrosim vignette
# **********************************************************

# *************************************************************
# File/Package Setup
# *************************************************************

# *** Install latest version of package (if necessary)
# install.packages("C:/Temp/rsyncrosim_0.1.0.tar.gz",repos=NULL)    # Download development version of rsyncrosim
# ***

library(rsyncrosim)
library(raster)

# Vignetter assumes that the following files are placed in the current working directory:
# stsim-3-1-1-x64.ssimpkg, initial-stratum.tif, initial-sclass.tif, initial-age.tif
getwd()   

# *************************************************************
# Start Session & Create Library
# *************************************************************

# Start a SyncroSim session
programFolder = "c:/gitprojects/syncrosim/_deploy_/current"#"C:/Program Files/SyncroSim"    # Directory in which SyncroSim was installed
mySession = session(programFolder)

# Add the ST-Sim module to this session
#addModule("stsim-3-1-1-x64.ssimpkg", mySession)

# Check stsim module is now loaded in the session
module(mySession)

# Create a new library (or load it if it already exists)
myLibrary = ssimLibrary(name="Demonstration Library", session=mySession)

# Display internal names of the library's datasheets
(librarySheetNames = datasheet(myLibrary, summary=T))

# Modify library datasheet to include input spatial files in backups
sheetData = datasheet(myLibrary, name="SSim_Backup")                       # Get the current backup settings for the library
sheetData = addRow(sheetData, data.frame(IncludeInput=T, IncludeOutput=F)) # Add a new row to this dataframe
saveDatasheet(myLibrary, data=sheetData, name="SSim_Backup")               # Save the new dataframe back to the library

# Backup the library before making changes
backup(myLibrary)

# *************************************************************
# Create Project
# *************************************************************

# Create a new SyncroSim project (or open it if it already exists)
myProject = project(myLibrary, project="Simple forest landscape")

# Display internal names of the project's datasheets
(projectSheetNames=datasheet(myProject, summary=T, optional=F))

# Edit the Project Definitions:

# Terminology
(sheetData = datasheet(myProject, "STSim_Terminology"))
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

# Transition Groups - make the Groups identical to the Types
transitionGroups = data.frame(Name=c("Fire", "Harvest", "Succession"))
saveDatasheet(myProject, transitionGroups, "STSim_TransitionGroup", force=T)

# Transition Types by Groups  - assign each Type to its Group
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

# Create/open a SyncroSim "No Harvest" scenario
myScenario = scenario(myProject, "No Harvest")

# Display the internal names of all the scenario datasheets
(scenarioSheetNames=subset(datasheet(myScenario, summary=T), scope=="scenario"))

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

#JH create rasters and write to disk
baseMap = raster(nrows=10,ncols=10,crs="+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",vals=0,
                 xmn=0,ymn=0,xmx=10,ymx=10)

writeRaster(baseMap+sample(seq(1:3),100,replace=T),"initial-sclass.tif",format="GTiff",overwrite=T)
writeRaster(baseMap+sample(seq(1:1),100,replace=T),"initial-stratum.tif",format="GTiff",overwrite=T)
writeRaster(baseMap+sample(seq(1:100),100,replace=T),"initial-age.tif",format="GTiff",overwrite=T)

# Check rasters (for initial conditions)
stratumTif = "initial-stratum.tif"
sclassTif = "initial-sclass.tif"
ageTif = "initial-age.tif"

rStratum = raster(stratumTif)
rSclass = raster(sclassTif)
rAge = raster(ageTif)

plot(rStratum)
plot(rSclass)
plot(rAge)

# Set Spatial initial conditions
sheetName = "STSim_InitialConditionsSpatial"
sheetData = data.frame(StratumFileName=stratumTif, StateClassFileName=sclassTif, AgeFileName=ageTif)
saveDatasheet(myScenario, sheetData, sheetName,append=F)
datasheet(myScenario, sheetName)

# Check rasters correctly loaded
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

backup(myLibrary)
# *************************************************************
# Run Scenarios & Get Results
# *************************************************************

# Run 2 scenarios
resultSummary = run(myProject, scenario=c("Harvest", "No Harvest"), jobs=4, summary=T)

str(resultSummary)
# Get the scenario IDs of the 2 result scenarios
resultIDNoHarvest = subset(resultSummary, parentID==scenarioId(myScenario))$scenarioId
resultIDHarvest = subset(resultSummary, parentID==scenarioId(myScenarioHarvest))$scenarioId

# View state class tabular output (for both scenarios combined)
outputStratumState = datasheet(myProject, scenario=c(resultIDNoHarvest,resultIDHarvest), name="STSim_OutputStratumState")

# View state class raster output (for the Harvest scenario only)
myRastersTimestep5 = datasheetRaster(myProject, scenario=resultIDHarvest, "STSim_OutputSpatialState", timestep=5)
myRastersTimestep5
plot(myRastersTimestep5)





