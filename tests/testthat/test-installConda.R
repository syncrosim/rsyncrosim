#myLibraryName <- file.path(tempdir(),"testlib")
#mySession <- session()
#myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
mySession <- session(myLibrary)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

#test_that("can installConda", {
  #expect_error(expect_error(installConda(mySession))) #only works if leave field empty, can't select a session object
  #expect_error(expect_error(installConda()))
#})

test_that("errors work", {
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(installConda("myLibrary"))
  expect_error(installConda(vector))
  expect_error(installConda(list))
  expect_error(installConda(character))
  expect_error(installConda(df))
  expect_error(installConda(library))
  expect_error(installConda(myProject))
  expect_error(installConda(myScenario))
  expect_error(installConda(myLibrary))
})

