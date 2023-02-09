test_that("can retrive scenario", {
  myLibrary1 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario1 <- scenario(myProject, scenario = "My Scenario")
  myScenario2 <- scenario(myProject, scenario = "New Scenario")

  #scenario argument
  expect_error(expect_error(scenario(myLibrary, scenario = "New Scenario")))
  expect_error(expect_error(scenario(myLibrary, scenario = "My Scenario")))
  expect_error(expect_error(scenario(myLibrary, scenario = 1)))
  expect_error(expect_error(scenario(myLibrary, scenario = c(1,2))))
  expect_error(expect_error(scenario(myLibrary, scenario = c("My Scenario","New Scenario"))))

  #ssimObject arguments
  expect_error(expect_error(scenario(myLibrary)))
  expect_equal(scenario(myLibrary1), scenario("C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim"))
  expect_error(expect_error(scenario(myScenario1)))
  expect_error(expect_error(scenario(myScenario2)))
  expect_error(expect_error(scenario(myProject)))

  #class and type
  expect_type(myScenario1, "S4")
  expect_s4_class(myScenario1, "Scenario")
  expect_type(myScenario2, "S4")
  expect_s4_class(myScenario2, "Scenario")

  #sourceScenario argument
  expect_error(expect_error(scenario(myLibrary, scenario = "Another Scenario", sourceScenario = 1, overwrite = TRUE)))
  expect_error(expect_error(scenario(myLibrary, scenario = "More Scenario", sourceScenario = 2, overwrite = TRUE)))

  #summary argument
  expect_equal(scenario(myLibrary), scenario(myLibrary, summary = TRUE))
  expect_equal(scenario(myProject), scenario(myProject, summary = TRUE))
  expect_equal(scenario(myScenario1), scenario(myScenario1, summary = FALSE))
  expect_equal(scenario(myLibrary, scenario = "New Scenario"), scenario(myScenario2, summary = FALSE))
  expect_s3_class(scenario(myLibrary, summary = TRUE), "data.frame")
  expect_type(scenario(myLibrary, summary = FALSE), "list")
  expect_s3_class(scenario(myScenario1, summary = TRUE), "data.frame")

  #forceElements argument
  expect_equal(scenario(myLibrary, summary = TRUE), scenario(myLibrary, summary = TRUE, forceElements = TRUE))
  expect_equal(scenario(myLibrary, summary = FALSE), scenario(myLibrary, summary = FALSE, forceElements = FALSE))
  expect_type(scenario(myLibrary, forceElements = TRUE), "list")

  #overwrite argument
  expect_equal(nrow(scenario(myLibrary, forceElements = TRUE)), 4)
  scenario(myLibrary, scenario = "overwrite")
  expect_equal(nrow(scenario(myLibrary, forceElements = TRUE)), 5)
  scenario(myLibrary, scenario = "overwrite", overwrite = TRUE)
  expect_equal(nrow(scenario(myLibrary, forceElements = TRUE)), 5)
})

test_that("errors work", {
  myLibrary1 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario1 <- scenario(myProject, scenario = "My Scenario")
  myScenario2 <- scenario(myProject, scenario = "New Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(scenario("myLibrary"))
  expect_error(scenario(vector))
  expect_error(scenario(list))
  expect_error(scenario(character))
  expect_error(scenario(df))
  expect_error(scenario(library))
  expect_error(scenario(C(myScenario1, myScenario2)))
  expect_error(scenario(mySession))
  expect_error(scenario(myLibrary, scenario = "Another Scenario", sourceScenario = 32))
  expect_error(scenario(myLibrary, scenario = myProject))

})
