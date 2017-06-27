retDir = getwd()
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")
#library(testthat)

test_that("Tests of Session", {
  mySsim = session() # Creates a session using the default installation of syncrosim
  expect_is(mySsim, "Session")
  expect_equal(file.exists(filepath(mySsim)),TRUE) # Lists the folder location of syncrosim session
  expect_output(str(version(mySsim)),"chr",fixed=T) # Lists the version of syncrosim session
  expect_equal(names(modules(mySsim)),c("name","displayName","version")) # Dataframe of the modules installed with this verions of SyncroSim.
  expect_equal(names(modules()),c("name","displayName","version")) # Dataframe of the modules installed with this verions of SyncroSim.
  expect_equal(names(model(mySsim)),c("name","description","version")) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
  expect_equal(names(model()),c("name","description","version")) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
  expect_equal(deleteModule("hi",mySession),list(hi="Module hi is not installed, so cannot be removed."))
  
  mySession = session(defaultModel="stsim",silent=F) #modify default session settings
  #defaultModel(mySession)="stsim" #prints SyncroSim command calls because printCmd=T
  expect_equal(defaultModel(mySession),"stsim")
  expect_equal(silent(mySession),F)
  silent(mySession)=T
  expect_equal(silent(mySession),T)
  expect_output(session(printCmd=T),"--list --models --csv")
})


test_that("Tests of command", {
  mySsim = session() # Creates a session using the default installation of syncrosim
  expect_equal(command("help")[1],"System Console [Arguments]")
  expect_equal(command(c("list","help"),mySsim)[1],"Lists existing items")
  expect_equal(command("--create --help")[1],"Creates an item")
  expect_equal(command(list(create=NULL,help=NULL))[1],"Creates an item")
  
  delete(paste0(getwd(),"/temp.ssim"),force=T)
  args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp.ssim"),model="hello:model-transformer")
  output = command(args)
  expect_equal(output[1],"The transformer 'hello:model-transformer' was not found.  You may need to install an additional module.")
})

test_that("Tests of Library", {
  myLibrary = ssimLibrary(name="temp") #create new library using default model
  expect_equal(file.exists(filepath(myLibrary)),TRUE)
  expect_equal(model(myLibrary)[["name"]],"stsim")
  expect_equal(delete(myLibrary,force=T),"saved")
  expect_equal(file.exists(filepath(myLibrary)),FALSE)
    
  myLibrary = ssimLibrary() # creates a default ssimLibrary called SsimLibrary.ssim in the current R working directory
  expect_equal(file.exists(filepath(myLibrary)),TRUE)
  expect_equal(name(myLibrary),"SsimLibrary")

  # With addons
  expect_equal(nrow(addons(myLibrary)),0)
  allAdds = addons(myLibrary,all=T)
  expect_equal(names(allAdds),c("name","displayName","enabled","currentVersion","minimumVersion"))
  expect_gt(nrow(allAdds),0)
  expect_equal(names(addons(mySession)),c("name","displayName","version","extends"))
  expect_equal(delete(myLibrary,force=T),"saved")

  cAdd =allAdds$name[1]
  #delete(myLibrary,force=T)
  myLibrary = ssimLibrary(name= "NewLibrary", addon=c(cAdd),session=mySession) 
  expect_equal(addons(myLibrary)$name,cAdd)
  expect_equal(disableAddon(myLibrary,cAdd)[[cAdd]],"saved")
  expect_equal(nrow(addons(myLibrary)),0)
  expect_equal(enableAddon(myLibrary,cAdd)[[cAdd]],"saved")
  expect_equal(addons(myLibrary)$name,cAdd)

  myLibrary = ssimLibrary() # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error
  expect_equal(file.exists(filepath(myLibrary)),TRUE)

  # Get/set the various properties of the library
  expect_is("session<-"(myLibrary,session()),"SsimLibrary")
  
  expect_equal(ssimUpdate(myLibrary),"The library has no unapplied updates.")
  expect_equal(names(ssimLibrary(myLibrary)),c("property","value"))
  expect_equal(class(ssimLibrary(myLibrary,summary=F))[1],"SsimLibrary")
  
  name(myLibrary)="Fred"
  expect_equal(name(myLibrary),"Fred")
  expect_equal(backup(myLibrary),"Backup complete.")
  expect_equal(dir.exists(paste0(filepath(myLibrary),".backup")),T)
  
  description(myLibrary) = "A new description.\nTry a linebreak." #NOTE: \n adds a linebreak to the description
  expect_equal(description(myLibrary)[2],"A new description.")
  
  owner(myLibrary) ="Fred"
  expect_equal(owner(myLibrary),"Fred")
  readOnly(myLibrary)=T
  expect_equal(readOnly(myLibrary),T)
  readOnly(myLibrary)=F
  expect_equal(readOnly(myLibrary),F)
  expect_equal(grepl("at",dateModified(myLibrary)),T)
  delete(myLibrary,force=T)
  
})

test_that("Tests of projects and scenarios", {
  ret=delete(paste0(getwd(),"/temp26.ssim"),force=T) #delete a library specified by a path
  ret=delete(paste0(getwd(),"/temp27.ssim"),force=T)
  myLib=ssimLibrary(name="temp26",session=mySession)
  myOtherLib = ssimLibrary(name="temp27",session=mySession)
  
  myOtherScn = scenario(myOtherLib,scenario="other")
  scenario(myOtherLib)
  delete(myOtherLib,scenario="other",force=T)
  scenario(myOtherLib)
  myOtherScn = scenario(myOtherLib,scenario="other2")
  
  project(myOtherLib)
  scenario(myOtherLib)
  
  myProject = project(myLib,project="temp")
  datasheet(myProject) #Only scope, name and displayName returned
  datasheet(myLib,project="temp") #same thing, but more system calls. Generally using ids/objects is faster than using names.
  str(datasheet(myProject,optional=T)) #all info
  #NOTE: data column only available for scenario scope datasheets
  #NOTE: dataInherited and dataSource columns added if there are dependencies. 
  
  project(myLib)
  
  #scenario(myLib,scenario=1) # Fail: need a name to create a scenario
  myScn = scenario(myLib,scenario="one") #Ok because only one project in the library.
  scenario(myLib)
  project(myLib)
  myProject = project(myLib,project="temp2")
  myScn = scenario(myLib,scenario="one") #Ok because only one scenario of this name occurs in the library.
  myScn = scenario(myProject,scenario="one") #Creates a new scenario called "one" in the second project.
  
  #myScn = scenario(myLib,scenario="one") #Fails because now there are two scenarios called "one" in the library.
  scenario(myLib)
  myScn = scenario(myProject,scenario="one",overwrite=T) #Overwrites existing scenario, assigns new id.
  scenario(myLib)
  myScn = scenario(myProject,scenario="two",overwrite=T,sourceScenario=1) #Can copy scenarios between projects.
  scenario(myLib)
  myScn = scenario(myProject,scenario="other",overwrite=T,sourceScenario=myOtherScn) #Can copy scenarios between libraries if sourceScenario is a scenario object.
  scenario(myLib)
  
  myOtherProject=project(myOtherLib,project="copy",sourceProject=myProject)#Can copy projects among libraries provided that sourceProject is a Project object.
  
  project(myLib)
  myOtherProject=project(myLib,project="copy",sourceProject=10)#Copy a project within the same library.
  project(myLib)
  myOtherProject=project(myLib,project="temp",sourceProject="temp2")#Warns that sourceProject is ignored because "temp" already exists.
  myOtherProject=project(myLib,project="copy2",sourceProject="temp2")#Copy a project by name
  project(myLib)
  
  scenario(myLib)
  projectId(myProject)
  delete(myProject,scenario="one",force=T)
  myScn = scenario(myProject,scenario="one",sourceScenario="one") #Ok because only one possible source
  myScn = scenario(myProject,scenario="one",sourceScenario="one") #Warns that sourceScenario will be ignored.
  #myScn = scenario(myProject,scenario="three",sourceScenario="one") #Fail if more than one scenario named sourceScenario in the library.
  scenarioId(myScn)
  scenario(myScn,summary=T) #return summary info
  
  aSheet = datasheet(myScn,"SSim_Files")#returns a datasheet
  str(aSheet)
  
  aSheet = datasheet(myScn,"SSim_Files",forceElements=T) #returns a list
  str(aSheet)
  
  someSheets = datasheet(myScn,c("SSim_Settings","SSim_Files")) #returns a list
  str(someSheets)
  
  allScns = scenario(myLib,summary=F)
  names(allScns)
  someSheets = datasheet(myLib,c("STSim_RunControl","STSim_Transition"),scenario=as.numeric(names(allScns))) #returns a list - each sheet contains scenario info if appropriate
  str(someSheets)
  someSheets = datasheet(allScns,c("STSim_RunControl","STSim_Transition")) #returns a list - each sheet contains scenario info if appropriate
  str(someSheets)
  
  aSheet = datasheet(myScn,"STSim_RunControl",scenario=1)#Warn of conflict between ssimObject and scenario arguments.
  aSheet = datasheet(myProject,"STSim_StateClass",project=1)#Warn of conflict between ssimObject and project arguments.
  anotherScn = scenario(myProject,"another scn")
  aSheet = datasheet(allScns,"STSim_RunControl",scenario=anotherScn)#Warn that project/scenario arguments are ignored when ssimObject is a list of Project/Scenario objects.
  
  myScn = scenario(myProject,scenario="one")
  runLog(myScn) #Returns message if the scenario is not a result scenario.
  
  #get/set properties
  name(myProject)
  name(myProject) = "New project name"
  name(myProject)
  
  name(myScn)
  name(myScn) = "New scn name"
  name(myScn)
  
  description(myProject) = "A new description.\nTry a linebreak." #NOTE: \n adds a linebreak to the description
  description(myProject) 
  description(myScn) = "Hi"
  description(myScn) 
  
  owner(myProject) ="Fred"
  owner(myProject)
  owner(myScn) ="Alice"
  owner(myScn)
  
  readOnly(myProject)=T
  readOnly(myProject)
  readOnly(myProject)=F
  readOnly(myProject) 
  
  readOnly(myScn)=T
  readOnly(myScn)
  
  dateModified(myProject)
  dateModified(myScn)
  parentId(myScn)#NA for scenarios that aren't results.
  
  myLib = ssimLibrary(myProject) #get parent library
  mySession = session(myProject) #get parent session
  myLib = ssimLibrary(myScn) #get parent library
  mySession = session(myScn) #get parent session
  myProject=project(myScn) #get parent project
  
  scenarioId(myScn)
  projectId(myScn)
  projectId(myProject)
  filepath(myScn)
  filepath(myProject)
  ssimUpdate(myScn)
  ssimUpdate(myProject)
  
  #test dependency, precedence setting
  scenario(myProject)
  targetScn = scenario(myProject,scenario="two")
  dependency(targetScn)
  dependency(targetScn,dependency=c("other","New scn name","another scn")) #elements of the dependency argument are ordered from lowest to highest precedence
  dependency(targetScn) #"another scn" was added last, so has highest precedence
  dependency(targetScn,dependency=c("another scn","New scn name")) #change the precedence of dependencies by adding them again.
  dependency(targetScn) #now "New scn name" has highest precedence.
  
  #print/show methods for SsimObjects and Sessions
  myScenario
  myProject
  myLibrary
  mySession
  
  
  #test delete - vectors of project/scenario/datasheet
  scenario(myLib)
  datasheet(myProject)
  delete(myLib, project=c(1,10),datasheet=c("STime_Chart","STime_DistributionType"),force=T)
  
  delete(myLib, scenario=c(6,7),force=T)
  scenario(myLib)
  delete(myLib, scenario=c("one","two"),force=T)
  scenario(myLib)
  
  project(myLib)
  delete(myLib,project=c(1,10),force=t)
  project(myLib)
  delete(myLib,project=c("copy","copy2"),force=T)
  project(myLib)
  
})

test_that("Tests of Project", {
  
myLibrary = ssimLibrary(name="New Lib5")
myProject = project(ssimLibrary=myLibrary, project="My new project name")

# Get a named list of existing projects
myProjects = project(myLibrary,summary=F,forceElements=T) # Each element in the list is named by a character version of the project ID
expect_output(str(myProjects),"List of 1")
expect_is(myProjects[[1]],"Project")

# Get an existing project. Assume that name uniquely identifies a single project - give error if not
myProject = project(myLibrary, project="My new project name")
expect_is(myProject,"Project")

# Get/set the project properties - for now we can only set the name
name(myProject) = "New project name"
expect_equal(name(myProject),"New project name")

expect_is(ssimLibrary(myProject),"SsimLibrary") # Returns a SyncroSimLibrary object for the project

# Delete projects
expect_output(delete(myLibrary, project=c("New project name","hi"),force=T),"Cannot remove the project hi from the library because it does not exist.")
myProjectNames = project(myLibrary)
expect_equal(names(myProjectNames),c("id","name"))
expect_equal(is.element("New project name",myProjectNames$name),FALSE)
unlink(filepath(myLibrary))
})

test_that("Tests of Scenario", {
  myLibrary = ssimLibrary(name="Another Lib")
  myProject = project(myLibrary,project="a project")
  myScenario = scenario(myProject, scenario="Scenario")
  expect_is(myScenario,"Scenario")  #returns dataframe
  
  myScenarios =scenario(myLibrary,summary=F,forceElements=T)
  expect_is(myScenarios[[1]],"Scenario")

  myScenario = scenario(myProject, scenario="My new scenario name")

  myScnNames  =   scenario(myLibrary)
  expect_equal(names(myScnNames),c("id","pid","name","isResult","parentScenarioID","owner","lastModified","readOnly"))
  expect_equal(myScnNames$name,c("Scenario","My new scenario name"))

  expect_is(scenarioId(myScenario),"numeric")

  # Get an existing scenario by ID
  expect_is(scenario(myLibrary, scenario=1),"Scenario") # By ID directly from the library - return a single scenario object

  # Get/set the scenario properties - i.e. name, owner, description and readOnly
  expect_equal(name(myScenario),"Another scenario")
  name(myScenario) = "New scenario name"
  expect_equal(name(myScenario),"New scenario name")

  expect_is(ssimLibrary(myScenario),"SsimLibrary")  # Returns a SyncroSimLibrary object for the scenario
  expect_is(projectId(myScenario),"numeric")  # Returns the project ID for the scenario

  #Get/set scenario properties
  expect_equal(readOnly(myScenario),FALSE)    # Returns TRUE/FALSE
  expect_equal(owner(myScenario),"Colin")
  expect_equal(description(myScenario),"My description")
  owner(myScenario)="Colin Daniel"
  expect_equal(owner(myScenario),"Colin Daniel")

  # Delete scenarios
  ret = delete(myLibrary, scenario=scenario(myLibrary)$scenarioId,force=T)
  expect_equal(nrow(scenario(myLibrary)),0)
  unlink(filepath(myLibrary))
})

setwd(retDir)
#setwd('..')
unlink("testLibs",recursive=T)
#getwd()
