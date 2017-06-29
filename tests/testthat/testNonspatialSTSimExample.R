library(plyr)
retDir = getwd()
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")

#used to test run, dependency and a few other things that require a working library. More basic functionality is tested in testExamples.R - avoid redundancy with that set of tests.
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

  sheetName = "STSim_StateClass"
  expect_warning(datasheet(myProject,name=sheetName,empty=F),"StateLabelXID depends on STSim_StateLabelX. You should load STSim_StateLabelX before setting STSim_StateClass.")
  
  sheetName = "STSim_StateLabelY"
  mySheet = data.frame(Name="All")
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  
  sheetName = "STSim_StateLabelX"
  mySheet = data.frame(Name=c('Coniferous','Deciduous','Mixed'))
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  
  sheetName = "STSim_StateClass"; mySheet = datasheet(myProject,name=sheetName,empty=T)
  expect_equal(levels(mySheet$StateLabelXID),c("Coniferous","Deciduous","Mixed"))
  mySheet[1:3,"StateLabelXID"]=levels(mySheet$StateLabelXID) #Valid values
  mySheet$StateLabelYID = levels(mySheet$StateLabelYID)[1] #Valid values
  mySheet$Name = paste0(mySheet$StateLabelXID,":",mySheet$StateLabelYID)
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  expect_equal(is.element("StateClassID",names(datasheet(myProject,sheetName,includeKey=T))),T) #include primary key for datasheet

  #***********************************
  # Transitions
  sheetName = "STSim_TransitionGroup"
  mySheet = data.frame(Name = c("Fire","Harvest","Succession"))
  ret = saveDatasheet(myProject,mySheet,name=sheetName)
  ret = saveDatasheet(myProject,mySheet,name="STSim_TransitionType")

  sheetName = "STSim_TransitionTypeGroup"
  mySheet = data.frame(TransitionTypeID = c("Fire","Harvest","Succession"))
  mySheet$TransitionGroupID = mySheet$TransitionTypeID
  ret = saveDatasheet(myProject,mySheet,name=sheetName)

  #****************
  # Age type
  sheetName = "STSim_AgeType"
  mySheet = data.frame(Frequency=1,MaximumAge=100)
  ret = saveDatasheet(myProject,mySheet,name=sheetName)

  #*************************************
  # Build Scenario That Contains Shared Parameters
  #*************************************
  myScenario = scenario(myProject,scenario="Dependency Scenario")
  
  #**************
  # Run control
  sheetName = "STSim_RunControl"
  mySheet = data.frame(MinimumIteration=1,MaximumIteration=2,MinimumTimestep=0,MaximumTimestep=10)
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)
  
  #**************************
  # Deterministic transitions
  sheetName = "STSim_DeterministicTransition"; mySheet = datasheet(myScenario,name=sheetName,optional=T,empty=T)
  mySheet=addRow(mySheet,data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Coniferous:All",AgeMin=21,Location="C1"))
  expect_equal(mySheet$Location,"C1")
  mySheet = addRow(mySheet,data.frame(StateClassIDSource="Deciduous:All",StateClassIDDest="Deciduous:All",Location="A1"))
  mySheet = addRow(mySheet,data.frame(StateClassIDSource="Mixed:All",StateClassIDDest="Mixed:All",AgeMin=11,Location="B1"))
  expect_equal(mySheet$AgeMin,c(21,NA,11))
  expect_equal(levels(mySheet$StateClassIDSource),c("Coniferous:All","Deciduous:All","Mixed:All"))
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)

  #*************************
  # Probabilistic transitions
  sheetName = "STSim_Transition"
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

  #********************
  #Initial conditions
  sheetName = "STSim_InitialConditionsNonSpatial"
  mySheet = data.frame(TotalAmount=100,NumCells=100)
  ret=saveDatasheet(myScenario,mySheet,name=sheetName)

  sheetName = "STSim_InitialConditionsNonSpatialDistribution"
  mySheet=data.frame(
    StateClassID = c("Coniferous:All","Deciduous:All","Mixed:All"),
    AgeMin = c(20,NA,11),
    AgeMax = c(100,10,20),
    RelativeAmount = c(20,40,40)
    )
  mySheet$StratumID = "Entire Forest"
  ret = saveDatasheet(myScenario,mySheet,name=sheetName)
  
  #*************************************
  # Add No Harvest Scenario
  #*************************************
  myScenario = scenario(myProject,scenario="No Harvest")

  ret = dependency(myScenario,dependency="Dependency Scenario") #set dependency
  expect_equal(dependency(myScenario)$name,"Dependency Scenario") #now there is a dependency 
  ret = dependency(myScenario,dependency="Dependency Scenario",remove=T,force=T)
  expect_equal(nrow(dependency(myScenario)),0) #dependency has been removed
  ret=dependency(myScenario,dependency="Dependency Scenario") #set dependency
  
  #******************
  # Transition targets
  sheetName = "STSim_TransitionTarget"
  mySheet=data.frame(TransitionGroupID = "Harvest",Amount=0)
  ret= saveDatasheet(myScenario,mySheet,name=sheetName)

  #*************************************
  # Add Harvest Scenario
  #*************************************
  myScenario = scenario(myProject,scenario="Harvest",sourceScenario="No Harvest")
  
  #******************
  # Transition targets
  sheetName = "STSim_TransitionTarget"
  mySheet=data.frame(TransitionGroupID = "Harvest",Amount=20)
  ret= saveDatasheet(myScenario,mySheet,name=sheetName)

  #********************************
  # Run scenarios
  #******************************
  myResults = run(myProject,scenario=c("Harvest","No Harvest"),jobs=4)
  expect_is(myResults[[1]],"Scenario")

  scenario(myProject)
  class(myScenario)
  otherResults = run(myScenario,jobs=4,summary=T)
  expect_is(otherResults,"data.frame")
  
  expect_output(runLog(myResults[[1]]),"STARTING SIMULATION") #displays and returns a multiline string
  expect_equal(parentId(myResults[[1]]),3)

  #********************************
  # See results
  #******************************
  sheetName = "STSim_OutputStratumState"
  mySQL = sqlStatements(groupBy=c("ScenarioID","Iteration","Timestep","StateLabelXID"),aggregate=c("Amount"))
  outStatesAllAges = datasheet(myResults,name=sheetName,sqlStatements=mySQL)
  expect_equal(setdiff(unique(outStatesAllAges$Timestep),seq(from=0,to=10)),numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$Iteration),seq(from=1,to=2)),numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$ParentName),c("Harvest","No Harvest")),character(0))
  expect_equal(setdiff(unique(outStatesAllAges$StateLabelXID),c("Coniferous","Deciduous","Mixed")),character(0))
  checkSums = ddply(outStatesAllAges,.(ParentName,Iteration,Timestep),summarize,Amount=sum(Amount))
  expect_equal(unique(checkSums$Amount),100)
})

#setwd('..')
setwd(retDir)
unlink("testLibs",recursive=T)
#getwd()
