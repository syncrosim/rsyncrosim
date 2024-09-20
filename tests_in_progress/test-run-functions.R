### ApexRMS
### 2024-07-11
### Below script tests the following functions:
### * run
### * runLog

# Setup ----
myLibraryPath <- "C:/gitprojects/rsyncrosim/tests_in_progress/test_library/spatial-example.ssim"
mySession <- session("C:/gitprojects/ssimbin3/")

# Tests ----
# Load existing library and runnable scenarios (14, 16)
myLib <- ssimLibrary(myLibraryPath, session = mySession)

myProj <- project(myLib, project=1)

scenario(myProj)

myScn <- scenario(myProj, scenario = 14)

myScn2 <- scenario(myProj, scenario = 16)

# Run
resultScn <- run(myScn)
runLog(resultScn)

resultScnMultiple <- run(myProj, scenario = c(14,16))

resultScnSummary <- run(myScn, summary = T)

resultScnSummaryMultiple <- run(myProj, scenario = c(14,16), summary = T)

# Old code below for run ----
test_that("can run", {
  installPackage("helloworldSpatial")
  myLibraryName <- file.path(tempdir(),"library")
  mySession <- session(printCmd=T)
  myLibrary1 <- ssimLibrary(name = myLibraryName,
                            session = mySession,
                            package = "helloworldSpatial",
                            template = "example-library",
                            overwrite = TRUE)
  myProject1 <- project(myLibrary1, project = "Definitions")
  myScenario1 <- scenario(myProject1, scenario = "My Scenario")

  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  myScenario2 <- scenario(myProject, scenario = "New Scenario")

  helloscenario <- scenario(myLibrary, summary = TRUE)
  helloresult <- helloscenario[helloscenario$IsResult == "Yes",]
  helloID <- helloresult$ScenarioId
  delete(myLibrary, scenario = helloID, force = TRUE)

  #ssimObject
  expect_error(expect_error(run(myLibrary1)))
  expect_error(expect_error(run(myProject1)))
  expect_error(expect_error(run(myScenario1)))
  expect_error(expect_error(run(c(myScenario, myScenario2))))
  #scenario
  expect_error(expect_error(run(myLibrary1, scenario = "My Scenario")))
  expect_error(expect_error(run(myLibrary1, scenario = 1)))
  expect_error(expect_error(run(myProject, scenario = c("My Scenario", "New Scenario"))))
  expect_error(expect_error(run(myProject, scenario = c(1, 32))))
  #summary
  expect_error(expect_error(run(myScenario1, summary = TRUE)))
  expect_equal(class(run(myScenario1, summary = FALSE)), class(run(myScenario1)))
  expect_type(run(myProject, summary = FALSE, jobs = 6), "list")
  expect_s4_class(run(myScenario1, summary = FALSE), "Scenario")
  expect_type(run(myScenario1, summary = TRUE), "list")
  #jobs
  expect_error(expect_error(run(myScenario, jobs = 6)))
  expect_error(expect_error(run(myScenario, jobs = "6")))
  #copyExternalInputs
  expect_error(expect_error(run(myScenario, jobs = 1, copyExternalInputs = TRUE)))
  expect_error(expect_error(run(myScenario, jobs = 6, copyExternalInputs = FALSE)))
  expect_error(expect_error(run(myScenario, jobs = 6, copyExternalInputs = TRUE)))
  expect_error(expect_error(run(myScenario, jobs = 1, copyExternalInputs = "TRUE")))
  #transformerName
  #expect_error(expect_error(run(myScenario, jobs = 6, transformerName = "firstModel")))  #transformerName throws error. Two transformers can't be run independently
  #forceElements
  expect_error(expect_error(run(myScenario, forceElements = TRUE)))
  expect_equal(class(run(myScenario, summary = TRUE, forceElements = TRUE)), class(run(myScenario, summary = TRUE)))
  expect_equal(class(run(myScenario, forceElements = FALSE)), class(run(myScenario)))
  expect_type(run(myScenario, forceElements = TRUE), "list")

 helloscenario <- scenario(myLibrary, summary = TRUE)
 helloresult <- helloscenario[helloscenario$IsResult == "Yes",]
 helloID <- helloresult$ScenarioId
 delete(myLibrary, scenario = helloID, force = TRUE)
 })

test_that("test errors", {
  installPackage("helloworldSpatial")
  myLibraryName <- file.path(tempdir(),"library")
  mySession <- session(printCmd=T)
  myLibrary1 <- ssimLibrary(name = myLibraryName,
                            session = mySession,
                            package = "helloworldSpatial",
                            template = "example-library",
                            overwrite = TRUE)
  myProject1 <- project(myLibrary1, project = "Definitions")
  myScenario1 <- scenario(myProject1, scenario = "My Scenario")

  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  myScenario2 <- scenario(myProject, scenario = "New Scenario")

  vector <- c(1,2,3)
  list <- list("My Scenario", "New Scenario")
  character <- "character"
  df <- data.frame(list(x = "My Scenario", y = "New Scenario"))


  #ssimObject
  expect_error((run("myLibrary1")))
  expect_error((run(myProect1)))
  expect_error((run(df)))
  expect_error((run(character)))
  #scenario
  expect_error((run(myLibrary1, scenario = myScenario)))
  expect_error((run(myLibrary1, scenario = "1")))
  expect_error((run(myLibrary1, scenario = list)))
  #summary
  expect_error((run(myScenario1, summary = "TRUE")))
  expect_error((run(myScenario1, summary = YES)))
  expect_error((run(myScenario1, summary = character)))
  expect_error((run(myScenario1, summary = vector)))
  #jobs
  expect_error((run(myScenario, jobs = "six")))
  #copyExternalInputs
  expect_error((run(myScenario, jobs = 6, copyExternalInputs = YES)))
  #transformerName
  expect_error((run(myScenario, jobs = 6, transformerName = "firstModel")))
  #forceElements
  expect_error((run(myScenario, forceElements = "TRUE")))
  expect_error((run(myScenario, forceElements = YES)))
})

# Old code below for runLog ----
test_that("can retrive runLog from Scenario", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"library")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library",
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  expect_type(runLog(myScenario), "character")
  expect_error(expect_error(runLog(myScenario)))
  result <- run(myScenario)
  expect_type(runLog(result), "character")
  expect_error(expect_equal(runLog(result), runLog(myScenario)))
})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"library")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library",
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))
  
  expect_error(runLog("myScenario"))
  expect_error(runLog(vector))
  expect_error(runLog(list))
  expect_error(runLog(character))
  expect_error(runLog(df))
  expect_error(runLog(Scenario))
  expect_error(runLog(myProject))
  expect_error(runLog(myLibrary))
  expect_error(runLog(mySession))
})

