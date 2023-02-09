test_that("can retrive session", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")


})
