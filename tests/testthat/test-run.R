
test_that("can run", {
  addPackage("helloworldSpatial")
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
  helloID <- helloresult$ScenarioID
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
 helloID <- helloresult$ScenarioID
 delete(myLibrary, scenario = helloID, force = TRUE)
 })

test_that("test errors", {
  addPackage("helloworldSpatial")
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
