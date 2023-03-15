test_that("updatePackage works", {
  mySession <- session()

  expect_error(updatePackage(listonly = TRUE), NA)
  expect_error(updatePackage(listonly = TRUE, name = NULL), NA)
  expect_error(updatePackage(session = NULL, listonly = TRUE), NA)
  expect_error(updatePackage(session = mySession, listonly = TRUE), NA)
  expect_error(updatePackage(session = NULL, listonly = "TRUE"), NA)
  expect_type(updatePackage(session = mySession, listonly = TRUE), "logical")
  expect_equal(updatePackage(name = "stsim", session = mySession), FALSE)
  expect_message(updatePackage(name = "stsim", session = mySession), "No updates available for package 'stsim'.")
  expect_equal(updatePackage(name = "stsim", session = mySession), updatePackage(name = "stsim", session = mySession, listonly = FALSE))
})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_updatePackage")
  myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         package = "helloworldSpatial",
                         forceUpdate = TRUE,
                         overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(updatepackage(session = myLibrary))
  expect_error(updatepackage(session = myProject))
  expect_error(updatepackage(session = myScenario))
  expect_error(updatePackage(session = mySession, name = stsim))
  expect_message(updatePackage(session = mySession, name = "ssim"), "ssim : The package is not installed.")
  expect_error(updatePackage(session = NULL, listonly = NO))

})
