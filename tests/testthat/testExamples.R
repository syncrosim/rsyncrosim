retDir = getwd()
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")

test_that("Tests of Session", {
  mySsim = session() # Creates a session using the default installation of syncrosim
  expect_is(mySsim, "Session")
  expect_equal(file.exists(filepath(mySsim)),TRUE) # Lists the folder location of syncrosim session
  expect_output(str(version(mySsim)),"chr [1:2]",fixed=T) # Lists the version of syncrosim session
  expect_equal(names(modules(mySsim)),c("name","displayName","version")) # Dataframe of the modules installed with this verions of SyncroSim.
  expect_equal(names(models(mySsim)),c("name","displayName","shortName")) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
  expect_output(deletModule("hi",mySsim),"Module hi is not installed, so cannot be removed.")
})


test_that("Tests of command", {
  mySsim = session() # Creates a session using the default installation of syncrosim
  expect_equal(command("help")[1],"System Console [Arguments]")
  expect_equal(command(c("list","help"),mySsim)[1],"Lists existing items")
})

test_that("Tests of Library", {
  mySsim = session() # Creates a session using the default installation of syncrosim
  myLibrary = ssimLibrary(name="stsim")
  expect_equal(file.exists(filepath(myLibrary)),TRUE)
  expect_equal(names(addons(myLibrary)),c("name","displayName","enabled","shortName")) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
  myLibrary = ssimLibrary(name= "NewLibrary.ssim", addon=c("stsim-ecological-departure"))
  expect_equal(is.element("stsim-ecological-departure",addons(myLibrary)$shortName),TRUE)
  unlink(filepath(myLibrary))

  myLibrary = ssimLibrary() # look for a single .ssim file in the current working dir of R; if none found, or more than one, then raise error
  expect_equal(file.exists(filepath(myLibrary)),TRUE)

  # Open an existing ST-Sim library on a SyncroSim connection
  #myLibrary = ssimLibrary(name="stsim", backup=TRUE)
  #expect_equal(file.exists(gsub(".ssim","_backup.ssim",filepath(myLibrary),fixed=T)),TRUE)
  #unlink(gsub(".ssim","_backup.ssim",filepath(myLibrary),fixed=T))

  # Get/set the various properties of the library
  expect_equal(file.exists(filepath(session(myLibrary))),TRUE)
  expect_is("session<-"(myLibrary,session()),"SsimLibrary")

  expect_equal(is.element(modelName(myLibrary),models(mySsim)$name),TRUE) # Returns the name of the library's model - can't change this once the library is created.
  expect_match(modelVersion(myLibrary),"Source Module Version:",fixed=T)   # Returns the version of the library's model

  enableAddons(myLibrary) = c("stsim-stock-flow")
  expect_equal(is.element("stsim-stock-flow",addons(myLibrary)$shortName),TRUE)
  disableAddons(myLibrary) = c("stsim-stock-flow")
  expect_equal(is.element("stsim-stock-flow",addons(myLibrary)$shortName),FALSE)
  session(myLibrary)=session(silent=T)
  expect_equal(ssimUpdate(myLibrary),"The library has no unapplied updates.")
  unlink(filepath(myLibrary))
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
  expect_equal(names(myScnNames),c("id","pid","name","isResult","owner","readOnly","lastModified","parentId"))
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
  ret = delete(myLibrary, scenario=scenario(myLibrary)$id,force=T)
  expect_equal(nrow(scenario(myLibrary)),0)
  unlink(filepath(myLibrary))
})

setwd(retDir)
#setwd('..')
unlink("testLibs",recursive=T)
#getwd()
