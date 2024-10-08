### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * command

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibraryName <- file.path(tempdir(), "testlib.ssim")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test commands
test_that("commands work", {
  expect_equal(command(c("create", "help"), session = mySession), command("--create --help", session = mySession))
  expect_equal(command(c("create", "help"), session = mySession), command(list(create = NULL, help = NULL), session = mySession))
  expect_error(command(c("create", "help"), session = mySession), NA)
  expect_error(command(list(installed = NULL), session = mySession, program = "SyncroSim.PackageManager.exe"), NA)
  expect_error(command(list(installed = NULL), session = mySession, program = "SyncroSim.Console.exe"), NA)
  # expect_error((command(list(installed = NULL), program = "SyncroSim.Server.exe")), NA)
  # expect_error(command(list(installed = NULL), session = mySession, program = "SyncroSim.Multiband.exe"), NA) # SyncroSim.Multiband.exe not installed for SyncroSim Studio
  expect_error((command(list(installed = NULL), session = mySession, program = "SyncroSim.PackageManager.exe")), NA)
  expect_error((command(list(installed = NULL), session = mySession, program = "SyncroSim.PackageManager.exe", wait = FALSE)), NA)
  expect_error((command(arg = list(create = NULL, help = NULL), session = mySession, program = "SyncroSim.PackageManager.exe", wait = FALSE, progName = "test")), NA)
})

# test that errors are caused by incorrect arguments
test_that("errors work", {
  expect_error(command(arg = list(installed = NULL), session = myLibrary, program = "SyncroSim.PackageManager.exe", wait = FALSE, progName = "test"))
  expect_error(command(list(installed = NULL), session = myProject, program = "SyncroSim.PackageManager.exe"))
  expect_error(command(list(installed = NULL), session = myScenario, program = "SyncroSim.PackageManager.exe", wait = FALSE))
  #expect_warning(expect_warning(expect_error(command(list(installed = NULL), session = "mySession", program = "SyncroSim.PackageManager.exe", progName = "test"))))
})
