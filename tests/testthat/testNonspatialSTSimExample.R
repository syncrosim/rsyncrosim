library(plyr)
retDir = getwd()
#setwd("..")
#getwd()
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")

# **********************************************************
# commandLineTutorial.R
# Following the steps in Leo's PowerShell script using rsyncrosim.
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.15
# **********************************************************
# devtools::document();devtools::load_all()

test_that("Test simple non-spatial STSim example", {
  #*************************************
  # Create the project definition
  myLibrary = ssimLibrary(model="stsim",name="ST-Sim-Command-Line.ssim")
  myProject = project(myLibrary,name="ST-Sim Demonstration")

  #***********************************
  # Cover types and state classes
  sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=T)
  expect_equal(nrow(mySheet),0)
  mySheet[1,"Name"]="Entire Forest"
  #NOTE: this syntax preserves types and factor levels, and adds new rows if necessary. mySheet$Name="Entire Forest" does not.
  ret = loadDatasheets(myProject,mySheet,name=sheetName)
  expect_equal(datasheet(myProject,name=sheetName),data.frame(Name="Entire Forest",stringsAsFactors=F))

  # Warns if lookups are not loaded, and returns a factor with 0 levels
  sheetName = "STSim_StateClass"
  expect_warning(datasheet(myProject,name=sheetName,empty=F),"StateLabelXID depends on STSim_StateLabelX. You should load STSim_StateLabelX before setting STSim_StateClass.")

  sheetName = "STSim_StateLabelY"
  #mySheet = datasheet(myProject,name=sheetName)
  #mySheet[1,"Name"]="All"
  mySheet = data.frame(Name="All")
  ret = loadDatasheets(myProject,mySheet,name=sheetName)

  sheetName = "STSim_StateLabelX"
  #mySheet = datasheet(myProject,name=sheetName,empty=F)
  #mySheet[1:3,"Name"]=c('Coniferous','Deciduous','Mixed')
  mySheet = data.frame(Name=c('Coniferous','Deciduous','Mixed'))
  ret = loadDatasheets(myProject,mySheet,name=sheetName)

  # Now lookups are loaded we can set StateClass
  sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
  expect_equal(levels(mySheet$StateLabelXID),c("Coniferous","Deciduous","Mixed"))
  mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
  mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
  mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
  ret = loadDatasheets(myProject,mySheet,name=sheetName)
  #mySheet = datasheet(myProject,name=sheetName);str(mySheet)

  #***********************************
  # Transitions
  #datasheets(myProject,scope="project")$name #See project scope datasheet names
  sheetName = "STSim_TransitionGroup"
  #mySheet = datasheet(myProject,name=sheetName,empty=T)
  #mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
  mySheet = data.frame(Name = c("Fire","Harvest","Succession"))
  ret = loadDatasheets(myProject,mySheet,name=sheetName)
  ret = loadDatasheets(myProject,mySheet,name="STSim_TransitionType")

  sheetName = "STSim_TransitionTypeGroup"
  #mySheet = datasheet(myProject,name=sheetName,empty=T)
  #mySheet[1:3,"TransitionTypeID"]=levels(mySheet$TransitionTypeID)
  #mySheet[1:3,"TransitionGroupID"]=levels(mySheet$TransitionGroupID)
  mySheet = data.frame(TransitionTypeID = c("Fire","Harvest","Succession"))
  mySheet$TransitionGroupID = mySheet$TransitionTypeID
  ret = loadDatasheets(myProject,mySheet,name=sheetName)

  #****************
  # Age type
  sheetName = "STSim_AgeType"
  #mySheet = datasheet(myProject,name=sheetName,empty=T)
  #mySheet[1,"Frequency"] = 1
  #mySheet[1,"MaximumAge"] = 100
  mySheet = data.frame(Frequency=1,MaximumAge=100)
  ret = loadDatasheets(myProject,mySheet,name=sheetName)

  #*************************************
  # Add No Harvest Scenario
  #*************************************
  myScenario = scenario(myProject,name="No Harvest")
  expect_is(myScenario,"Scenario")
  # NOTE: To be consistent with project() I have used name/id in scenario().
  expect_equal(names(datasheets(myScenario,scope="scenario")),c("name","displayName","dataScope","isOutput"))

  #**************
  # Run control
  sheetName = "STSim_RunControl"
  #mySheet = datasheet(myScenario,name=sheetName,empty=T)
  #mySheet[1,"MinimumIteration"] = 1
  #mySheet[1,"MaximumIteration"] = 40
  #mySheet[1,"MinimumTimestep"] = 0
  #mySheet[1,"MaximumTimestep"] = 50
  mySheet = data.frame(MinimumIteration=1,MaximumIteration=2,MinimumTimestep=0,MaximumTimestep=10)
  ret = loadDatasheets(myScenario,mySheet,name=sheetName)

  #**************************
  # Deterministic transitions
  # devtools::document();devtools::load_all()
  sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  addRows(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1")
  expect_equal(mySheet$Location,"C1")
  addRows(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1")
  addRows(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1")
  expect_equal(mySheet$AgeMin,c(21,NA,11))
  expect_equal(levels(mySheet$StateClassIDSource),c("Coniferous:All","Deciduous:All","Mixed:All"))
  ret = loadDatasheets(myScenario,mySheet,name=sheetName)

  #*************************
  # Probabilistic transitions
  sheetName = "STSim_Transition"
  #mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  #addRows(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Fire",Probability=0.01)
  #addRows(mySheet)=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Harvest",Probability=1,AgeMin=40)
  #addRows(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Fire",Probability=0.002)
  #addRows(mySheet)=data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All",
  #                            TransitionTypeID="Succession",Probability=0.1,AgeMin=10)
  #addRows(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Fire",Probability=0.005)
  #addRows(mySheet)=data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All",
  #                            TransitionTypeID="Succession",Probability=0.1,AgeMin=20)
  mySheet = data.frame(
    StateClassIDSource=c("Coniferous:All","Coniferous:All","Deciduous:All","Deciduous:All","Mixed:All","Mixed:All"),
    StateClassIDDest=c("Deciduous:All","Deciduous:All","Deciduous:All","Mixed:All","Coniferous:All","Deciduous:All"),
    TransitionTypeID = c("Fire","Harvest","Fire","Succession","Succession","Fire"),
    Probability = c(0.01,1,0.002,0.1,0.1,0.005),
    AgeMin= c(NA,40,NA,10,20,NA)
    )
  mySheet$StratumIDSource="Entire Forest"
  mySheet$StratumIDDest = "Entire Forest"
  ret = loadDatasheets(myScenario,mySheet,name=sheetName)
  #mySheet = datasheet(myScenario,name=sheetName,optional=T); mySheet #check what happened

  #********************
  #Initial conditions
  # devtools::document();devtools::load_all()
  sheetName = "STSim_InitialConditionsNonSpatial"
  #mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
  #mySheet[1,"TotalAmount"]=1000
  #mySheet[1,"NumCells"]=1000
  mySheet = data.frame(TotalAmount=100,NumCells=100)
  ret=loadDatasheets(myScenario,mySheet,name=sheetName)

  sheetName = "STSim_InitialConditionsNonSpatialDistribution"
  #mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  #addRows(mySheet)=data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20)
  #addRows(mySheet)=data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40)
  #addRows(mySheet)=data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40)
  mySheet=data.frame(
    StateClassID = c("Coniferous:All","Deciduous:All","Mixed:All"),
    AgeMin = c(20,NA,11),
    AgeMax = c(100,10,20),
    RelativeAmount = c(20,40,40)
    )
  mySheet$StratumID = "Entire Forest"
  ret = loadDatasheets(myScenario,mySheet,name=sheetName)

  #******************
  # Transition targets
  sheetName = "STSim_TransitionTarget"
  #mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
  #mySheet[1,"TransitionGroupID"]="Harvest"
  #mySheet[1,"Amount"]=0
  mySheet=data.frame(TransitionGroupID = "Harvest",Amount=0)
  ret= loadDatasheets(myScenario,mySheet,name=sheetName)

  #*************************************
  # Add Harvest Scenario
  #*************************************
  # devtools::document();devtools::load_all()
  # deleteScenarios(myProject,"Harvest",force=T)
  myScenario = scenario(myProject,name="Harvest",sourceScenario="No Harvest")
  expect_is(myScenario,"Scenario")
  # Copies "No Harvest" scenario to new "Harvest" scenario

  #******************
  # Transition targets
  sheetName = "STSim_TransitionTarget"
  #mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
  #mySheet[1,"TransitionGroupID"]="Harvest"
  #mySheet[1,"Amount"]=20
  mySheet=data.frame(TransitionGroupID = "Harvest",Amount=20)
  ret= loadDatasheets(myScenario,mySheet,name=sheetName)

  #********************************
  # Run scenarios
  #******************************
  # devtools::document();devtools::load_all()

  myResults = run(myProject,scenario=c("Harvest","No Harvest"),jobs=2)
  expect_is(myResults[[1]],"Scenario")

  #********************************
  # See results
  #******************************
  # devtools::document();devtools::load_all()
  sheetName = "STSim_OutputStratumState"
  names(datasheet(myResults,name=sheetName,lookupsAsFactors=F,empty=T)) #Get column names without getting data
  mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","StateLabelXID"),aggregate=c("Amount"))
  mySQL # A list of SELECT and GROUP BY SQL statements passed to SQLite.
  outStatesAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
  str(outStatesAllAges) #Much faster: fewer lookups and fewer records.
  expect_equal(setdiff(unique(outStatesAllAges$Timestep),seq(from=0,to=10)),numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$Iteration),seq(from=1,to=2)),numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$ScenarioParent),c("Harvest","No Harvest")),character(0))
  expect_equal(setdiff(unique(outStatesAllAges$StateLabelXID),c("Coniferous","Deciduous","Mixed")),character(0))
  checkSums = ddply(outStatesAllAges,.(ScenarioParent,Iteration,Timestep),summarize,Amount=sum(Amount))
  expect_equal(unique(checkSums$Amount),100)


  ####
  #misc example checks
  # Get a list of existing results scenarios for a particular project
  expect_equal(nrow(scenarios(myProject, results=TRUE,names=T)),2)




})

#setwd('..')
setwd(retDir)
unlink("testLibs",recursive=T)
