
test_that("can retrive package", {
  myLibraryName <- file.path(tempdir(),"testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(package(myLibrary), "list")
  expect_s3_class(package(myLibrary), "data.frame")
  expect_error(expect_error(package(myLibrary)))
  expect_equal(package(myLibrary, installed = TRUE), package(myLibrary, installed = FALSE))
  expect_equal(package(myLibrary, installed = "BASE"), package(myLibrary, installed = FALSE))
  expect_equal(package(myLibrary, installed = TRUE), package(myLibrary, installed = TRUE, listTemplates = "stsimBurnP3Plus"))

  expect_type(package(mySession), "list")
  expect_s3_class(package(mySession), "data.frame")
  expect_error(expect_error(package(mySession)))
  expect_error(expect_equal(package(mySession, installed = TRUE), package(mySession, installed = FALSE)))
  expect_error(expect_equal(package(mySession, installed = "BASE"), package(mySession, installed = FALSE)))
  expect_error(expect_equal(package(mySession, installed = TRUE), package(mySession, installed = TRUE, listTemplates = "stsimBurnP3Plus")))
  expect_equal(nrow(package(mySession, installed = TRUE)), 28)
  expect_equal(nrow(package(mySession, installed = TRUE, listTemplates = "stsimBurnP3Plus")), 1)
})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(),"testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(package("myLibrary"))
  expect_error(package(vector))
  expect_error(package(list))
  expect_error(package(character))
  expect_error(package(df))
  expect_error(package(library))
  expect_error(package(myProject))
  expect_error(package(myScenario))

})
