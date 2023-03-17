test_that("can retrive projectId from Scenario", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(projectId(myScenario), "double")
  expect_error(expect_error(projectId(myScenario)))
  expect_type(projectId(myProject), "double")
  expect_error(expect_error(projectId(myProject)))
})

test_that("errors work", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(projectId("myScenario"))
  expect_error(projectId(vector))
  expect_error(projectId(list))
  expect_error(projectId(character))
  expect_error(projectId(df))
  expect_error(projectId(Scenario))
  expect_error(projectId(myLibrary))
  expect_error(projectId(mySession))
})
