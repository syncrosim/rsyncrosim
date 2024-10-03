### ApexRMS
### 2024-07-11
### Below script tests the following functions:
### * ssimLibrary
### * dateModified
### * description
### * filepath
### * info
### * name
### * owner
### * readOnly
### * useConda


# Setup ----
myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession <- session("C:/gitprojects/ssimbin3/")

# Tests ----
# Create new library
ssimLibrary(myLibraryName1)

# Create new library in custom session
myLib <- ssimLibrary(myLibraryName1, session = mySession)

# Check date modified
dateModified(myLib)

# Check and modify description
description(myLib)
description(myLib) <- "test"
description(myLib)

# Check filepath
filepath(myLib)

# Check and modify name
name(myLib)
name(myLib) <- "New Test Lib Name"
name(myLib)

# Check and modify owner
owner(myLib)
owner(myLib) <- "ApexRMS"
owner(myLib)

# Check and modify readOnly
readOnly(myLib)
readOnly(myLib) <- TRUE
readOnly(myLib)

# Check and modify useConda
useConda(myLib)
useConda(myLib) <- TRUE
useConda(myLib)

# Check info and ensure it matches above properties 
# (filepath, name, owner, readonly, useConda)
info(myLib)

# Overwrite existing library
myLib <- ssimLibrary(myLibraryName1, session = mySession, overwrite = T)

# Check that info is now default
info(myLib)

# Add package to library on creation
myLib <- ssimLibrary(myLibraryName1, session = mySession, 
                     packages = "stsim", overwrite = T)
packages(myLib)

# Check that use conda is set on initiation
myLib <- ssimLibrary(myLibraryName1, session = mySession, 
                     packages = "stsim", overwrite = T,
                     useConda = T)
useConda(myLib)

# Check that library summary is returned (returns info instead of object)
myLibSummary<- ssimLibrary(myLibraryName1, session = mySession, 
                           packages = "stsim", overwrite = T,
                           summary = T)
myLibSummary

# Old tests for ssimLibrary ----
test_that("can retrive scenario", {
  installPackage("stsimsf")
  installPackage("stsimBurnP3Plus")

  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibraryName1 <- file.path(tempdir(),"other")

  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  #name argument
  expect_error(ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim"), NA)
  expect_error(ssimLibrary(name = myLibraryName), NA)
  expect_error(ssimLibrary(name = myLibrary), NA)
  expect_error(ssimLibrary(name = myProject), NA)
  expect_error(ssimLibrary(name = myScenario), NA)

  #summary arguments
  expect_error(ssimLibrary(name = myLibraryName, summary = TRUE), NA)
  expect_equal(ssimLibrary(name = myScenario), ssimLibrary(name = myScenario, summary = FALSE))
  expect_equal(class(ssimLibrary(name = myScenario)), class(ssimLibrary(name = myScenario, summary = FALSE)))

  #class and type
  expect_type(ssimLibrary(name = myLibraryName, summary = TRUE), "list")
  expect_s3_class(ssimLibrary(name = myLibraryName, summary = TRUE), "data.frame")
  expect_type(ssimLibrary(name = myLibraryName, summary = FALSE), "S4")
  expect_s4_class(ssimLibrary(name = myLibraryName, summary = FALSE), "SsimLibrary")

  #package argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim"), NA)
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "helloworldSpatial"), NA)
  expect_equal(ssimLibrary(name = myLibraryName1, package = "stsim"), ssimLibrary(name = myLibraryName1))

  #session argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim", session = mySession), NA)
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "helloworldSpatial", session = NULL), NA)

  #addon argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim", session = mySession, addon = c("stsimsf", "stsimBurnP3Plus")), NA)
  expect_error(ssimLibrary(name = myLibraryName1, summary = FALSE, package = "stsim", session = NULL, addon = "stsimsf"), NA)

  #template argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim", session = mySession, template = "spatial-example"), NA)
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "helloworldSpatial", session = NULL, template = "example-library"), NA)

  #forceUpdate argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim", session = mySession, forceUpdate = TRUE), NA)
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "helloworldSpatial", session = NULL, forceUpdate = FALSE), NA)

  #overwrite argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim", session = mySession, overwrite = TRUE), NA)
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "helloworldSpatial", session = NULL, forceUpdate = FALSE, overwrite = TRUE), NA)

  #useConda argument
  expect_error(ssimLibrary(name = myLibraryName1, summary = TRUE, package = "stsim", session = mySession, useConda = TRUE), NA)
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "helloworldSpatial", session = NULL, forceUpdate = FALSE, overwrite = TRUE, useConda = FALSE), NA)

})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibraryName1 <- file.path(tempdir(),"other")

  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  myScenario1 <- scenario(myProject, scenario = "My Scenario")
  myScenario2 <- scenario(myProject, scenario = "New Scenario")


  expect_error(ssimLibrary(library))
  expect_error(ssimLibrary(C(myScenario1, myScenario2)))
  expect_error(ssimLibrary(mySession))
  expect_error(ssimLibrary(name = mySession, summary = FALSE, package = "helloworldSpatial", session = NULL, forceUpdate = FALSE, overwrite = TRUE))
  expect_error(ssimLibrary(name = myScenario, summary = NO, package = "helloworldSpatial", session = NULL))
  expect_error(ssimLibrary(name = myScenario, summary = FALSE, package = "hello", session = NULL, forceUpdate = FALSE, overwrite = TRUE, useConda = TRUE))
  expect_error(ssimLibrary(name = myLibraryName1, summary = FALSE, package = "stsim", session = NULL, addon = stsimsf, forceUpdate = FALSE, overwrite = TRUE))

})
