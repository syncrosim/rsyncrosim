test_that("can retrive parentId from Scenario", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(parentId(myScenario), "logical")
  expect_error(expect_error(parentId(myScenario)))
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

  expect_error(parentId("myScenario"))
  expect_error(parentId(vector))
  expect_error(parentId(list))
  expect_error(parentId(character))
  expect_error(parentId(df))
  expect_error(parentId(Scenario))
  expect_error(parentId(myProject))
  expect_error(parentId(myLibrary))
  expect_error(parentId(mySession))
})
