### ApexRMS
### 2024-09-20
### Below script tests the following functions:
### * backup

# Setup ----
myLibraryName <- file.path(tempdir(), "testlib")
# mySession <- session("C:/gitprojects/ssimbin3/")
mySession <- session("C:/Program Files/SyncroSim Studio")


# define library, project, and scenario
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----
# backup library
expected1 <- backup(myLibrary)

# backup project
expected2 <- backup(myProject)

# backup scenario
expected3 <- backup(myScenario)

# check core_Backup datasheet
ds <- datasheet(myScenario, name = "core_Backup")
#ds <- (ds$IncludeInput = NA)

# make non-Ssim objects to test with
vector <- c(1, 2, 3)
list <- list(1, 2, 3)
character <- "character"
df <- data.frame(list(x = 1, y = 2, z  = 3))

# test that all Ssim objects can be backed up
test_that("can backup all SsimObjects", {
  expect_equal(expected1, TRUE)
  expect_equal(expected2, TRUE)
  expect_equal(expected3, TRUE)
  expect_type(expected1, "logical")
  expect_type(expected2, "logical")
  expect_type(expected3, "logical")
})

# test that non-Ssim objects will cause errors
test_that("errors work", {
  expect_error(backup("myLibrary"))
  expect_error(backup(vector))
  expect_error(backup(list))
  expect_error(backup(character))
  expect_error(backup(df))
  expect_error(backup(library))
})
