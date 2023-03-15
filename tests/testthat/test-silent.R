
test_that("can silent all SsimObjects", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_equal(silent(mySession), TRUE)
  expect_type(silent(mySession), "logical")
  expect_error(expect_error(silent(mySession) <- FALSE))
  expect_equal(silent(mySession), FALSE)
  expect_type(silent(mySession), "logical")

  expect_equal(silent(session("C:/Program Files/SyncroSim")), TRUE)
  expect_type(silent(session("C:/Program Files/SyncroSim")), "logical")
  #expect_error(expect_error(silent(session("C:/Program Files/SyncroSim")) <- FALSE))
  #expect_equal(silent(session("C:/Program Files/SyncroSim")), FALSE)
  #expect_type(silent(session("C:/Program Files/SyncroSim")), "logical")

})

test_that("errors work", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(silent(myScenario))
  expect_error(silent(myLibrary))
  expect_error(silent(myProject))
  expect_error(silent(vector))
  expect_error(silent(list))
  expect_error(silent(df))
  expect_error(silent(library))
  expect_error(silent(myScenario, TRUE))
  expect_error(silent(mySession) <- "TRUE")
})
