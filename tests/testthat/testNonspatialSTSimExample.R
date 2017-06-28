library(plyr)
retDir = getwd()
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")

test_that("Test simple non-spatial STSim example", {
  #*************************************
  # Create the project definition
  libPath = paste0(getwd(),"/ST-Sim-Command-Line.ssim")
  ret=delete(myLibrary,force=T)
  myLibrary = ssimLibrary(name=libPath)
  myProject = project(myLibrary,project="ST-Sim Demonstration")

  #***********************************
  # Cover types and state classes
  sheetName = "STSim_Stratum"; mySheet = datasheet(myProject,name=sheetName,empty=F,optional=T)
  mySheet[1,"Name"]="Entire Forest"
  mySheet[1,"Description"]="Another description"
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  expect_equal(names(datasheet(myProject,name=sheetName,empty=T,optional=F)),"Name") #returns only truly optional columns
  expect_equal(datasheet(myProject,name=sheetName,empty=F,optional=F)$Description,"Another description") #returns optional columns and columns with data
  expect_equal(names(datasheet(myProject,name=sheetName,empty=F,optional=T)),c("Name","ID","Color","Legend","Description")) #returns all columns

  #RESUME HERE  
  # Warns if lookups are not loaded, and returns a factor with 0 levels
  sheetName = "STSim_StateClass"
  expect_warning(datasheet(myProject,name=sheetName,empty=F),"StateLabelXID depends on STSim_StateLabelX. You should load STSim_StateLabelX before setting STSim_StateClass.")
  
  sheetName = "STSim_StateLabelY"
  #mySheet = datasheet(myProject,name=sheetName)
  #mySheet[1,"Name"]="All"
  mySheet = data.frame(Name="All")
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  
  sheetName = "STSim_StateLabelX"
  #mySheet = datasheet(myProject,name=sheetName,empty=F)
  #mySheet[1:3,"Name"]=c('Coniferous','Deciduous','Mixed')
  mySheet = data.frame(Name=c('Coniferous','Deciduous','Mixed'))
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  
  # Now lookups are loaded we can set StateClass
  sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
  expect_equal(levels(mySheet$StateLabelXID),c("Coniferous","Deciduous","Mixed"))
  mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
  mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
  mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  #mySheet = datasheet(myProject,name=sheetName);str(mySheet)
  
  #***********************************
  # Cover types and state classes

  
  
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
  #subset(datasheet(myProject),scope=="project")$name #See project scope datasheet names
  sheetName = "STSim_TransitionGroup"
  #mySheet = datasheet(myProject,name=sheetName,empty=T)
  #mySheet[1:3,"Name"]=c("Fire","Harvest","Succession")
  mySheet = data.frame(Name = c("Fire","Harvest","Succession"))
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  ret = saveDatasheet(myProject,mySheet,name="STSim_TransitionType")

  sheetName = "STSim_TransitionTypeGroup"
  #mySheet = datasheet(myProject,name=sheetName,empty=T)
  #mySheet[1:3,"TransitionTypeID"]=levels(mySheet$TransitionTypeID)
  #mySheet[1:3,"TransitionGroupID"]=levels(mySheet$TransitionGroupID)
  mySheet = data.frame(TransitionTypeID = c("Fire","Harvest","Succession"))
  mySheet$TransitionGroupID = mySheet$TransitionTypeID
  ret = saveDatasheet(myProject,mySheet,name=sheetName)

  #****************
  # Age type
  sheetName = "STSim_AgeType"
  #mySheet = datasheet(myProject,name=sheetName,empty=T)
  #mySheet[1,"Frequency"] = 1
  #mySheet[1,"MaximumAge"] = 100
  mySheet = data.frame(Frequency=1,MaximumAge=100)
  ret = saveDatasheet(myProject,mySheet,name=sheetName)

  #*************************************
  # Add No Harvest Scenario
  #*************************************
  myScenario = scenario(myProject,scenario="No Harvest")
  expect_is(myScenario,"Scenario")
  expect_equal(names(datasheet(myScenario)),c("scope","module","name","displayName","isSingle","isOutput"))

  #**************
  # Run control
  sheetName = "STSim_RunControl"
  #mySheet = datasheet(myScenario,name=sheetName,empty=T)
  #mySheet[1,"MinimumIteration"] = 1
  #mySheet[1,"MaximumIteration"] = 40
  #mySheet[1,"MinimumTimestep"] = 0
  #mySheet[1,"MaximumTimestep"] = 50
  mySheet = data.frame(MinimumIteration=1,MaximumIteration=2,MinimumTimestep=0,MaximumTimestep=10)
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)

  #**************************
  # Deterministic transitions
  # devtools::document();devtools::load_all()
  sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  mySheet=addRows(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1"))
  expect_equal(mySheet$Location,"C1")
  mySheet = addRows(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1"))
  mySheet = addRows(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1"))
  expect_equal(mySheet$AgeMin,c(21,NA,11))
  expect_equal(levels(mySheet$StateClassIDSource),c("Coniferous:All","Deciduous:All","Mixed:All"))
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)

  #*************************
  # Probabilistic transitions
  sheetName = "STSim_Transition"
  #mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  #mySheet=addRows(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Fire",Probability=0.01))
  #mySheet=addRows(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Harvest",Probability=1,AgeMin=40))
  #mySheet=addRows(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Fire",Probability=0.002))
  #mySheet=addRows(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Mixed:All",
  #                            TransitionTypeID="Succession",Probability=0.1,AgeMin=10))
  #mySheet=addRows(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Deciduous:All",
  #                            TransitionTypeID="Fire",Probability=0.005))
  #mySheet=addRows(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Coniferous:All",
  #                            TransitionTypeID="Succession",Probability=0.1,AgeMin=20))
  mySheet = data.frame(
    StateClassIDSource=c("Coniferous:All","Coniferous:All","Deciduous:All","Deciduous:All","Mixed:All","Mixed:All"),
    StateClassIDDest=c("Deciduous:All","Deciduous:All","Deciduous:All","Mixed:All","Coniferous:All","Deciduous:All"),
    TransitionTypeID = c("Fire","Harvest","Fire","Succession","Succession","Fire"),
    Probability = c(0.01,1,0.002,0.1,0.1,0.005),
    AgeMin= c(NA,40,NA,10,20,NA)
    )
  mySheet$StratumIDSource="Entire Forest"
  mySheet$StratumIDDest = "Entire Forest"
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)
  #mySheet = datasheet(myScenario,name=sheetName,optional=T); mySheet #check what happened

  #********************
  #Initial conditions
  # devtools::document();devtools::load_all()
  sheetName = "STSim_InitialConditionsNonSpatial"
  #mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
  #mySheet[1,"TotalAmount"]=1000
  #mySheet[1,"NumCells"]=1000
  mySheet = data.frame(TotalAmount=100,NumCells=100)
  ret=saveDatasheet(myScenario,mySheet,name=sheetName)

  sheetName = "STSim_InitialConditionsNonSpatialDistribution"
  #mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  #mySheet=addRows(mySheet,data.frame(StateClassID="Coniferous:All",AgeMin=20,AgeMax=100,RelativeAmount=20))
  #mySheet=addRows(mySheet,data.frame(StateClassID="Deciduous:All",AgeMax=10,RelativeAmount=40))
  #mySheet=addRows(mySheet,data.frame(StateClassID="Mixed:All",AgeMin=11,AgeMax=20,RelativeAmount=40))
  mySheet=data.frame(
    StateClassID = c("Coniferous:All","Deciduous:All","Mixed:All"),
    AgeMin = c(20,NA,11),
    AgeMax = c(100,10,20),
    RelativeAmount = c(20,40,40)
    )
  mySheet$StratumID = "Entire Forest"
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)

  #******************
  # Transition targets
  sheetName = "STSim_TransitionTarget"
  #mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
  #mySheet[1,"TransitionGroupID"]="Harvest"
  #mySheet[1,"Amount"]=0
  mySheet=data.frame(TransitionGroupID = "Harvest",Amount=0)
  ret= saveDatasheet(myScenario,mySheet,name=sheetName)

  #*************************************
  # Add Harvest Scenario
  #*************************************
  # devtools::document();devtools::load_all()
  # delete(myProject,scenario="Harvest",force=T)
  myScenario = scenario(myProject,scenario="Harvest",sourceScenario="No Harvest")
  expect_is(myScenario,"Scenario")
  # Copies "No Harvest" scenario to new "Harvest" scenario

  #******************
  # Transition targets
  sheetName = "STSim_TransitionTarget"
  #mySheet = datasheet(myScenario,name=sheetName,optional=F,empty=T)
  #mySheet[1,"TransitionGroupID"]="Harvest"
  #mySheet[1,"Amount"]=20
  mySheet=data.frame(TransitionGroupID = "Harvest",Amount=20)
  ret= saveDatasheet(myScenario,mySheet,name=sheetName)

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
  #names(datasheet(myResults,name=sheetName,lookupsAsFactors=F,empty=T)) #Get column names without getting data
  mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","StateLabelXID"),aggregate=c("Amount"))
  #mySQL # A list of SELECT and GROUP BY SQL statements passed to SQLite.
  outStatesAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
  #str(outStatesAllAges) #Much faster: fewer lookups and fewer records.
  expect_equal(setdiff(unique(outStatesAllAges$Timestep),seq(from=0,to=10)),numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$Iteration),seq(from=1,to=2)),numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$ScenarioParent),c("Harvest","No Harvest")),character(0))
  expect_equal(setdiff(unique(outStatesAllAges$StateLabelXID),c("Coniferous","Deciduous","Mixed")),character(0))
  checkSums = ddply(outStatesAllAges,.(ScenarioParent,Iteration,Timestep),summarize,Amount=sum(Amount))
  expect_equal(unique(checkSums$Amount),100)

  ?sqlStatements
  ####
  #misc example checks
  # Get a list of existing results scenarios for a particular project
  expect_equal(nrow(scenario(myProject, results=TRUE)),2)

  #RESUME HERE - test datasheet stuff from examples.R


})

#setwd('..')
setwd(retDir)
unlink("testLibs",recursive=T)
#getwd()
