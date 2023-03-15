myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession1 <- session()
mySession2 <- session()
myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package = "burnP3Plus", addon = "burnP3PlusCell2Fire", overwrite = TRUE)
myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "stsim", overwrite = TRUE)
myProject <- project(myLibrary1, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

test_that("can disableAddon all SsimObjects", {

  expect_message(enableAddon(myLibrary1, "burnP3PlusCell2Fire"), "burnP3PlusCell2Fire is already enabled.")
  disableAddon(myLibrary1, "burnP3PlusCell2Fire")
  expect_message(enableAddon(myLibrary1, "burnP3PlusCell2Fire"), "Addon <burnP3PlusCell2Fire> enabled")
  disableAddon(myLibrary1, "burnP3PlusCell2Fire")
  expect_error(expect_error(enableAddon(myLibrary1, c("burnP3PlusCell2Fire", "burnP3PlusPrometheus"))))

  expect_message(enableAddon(myLibrary2, "stsimBurnP3Plus"), "Addon <stsimBurnP3Plus> enabled")
  expect_message(enableAddon(myLibrary2, "stsimBurnP3Plus"), "stsimBurnP3Plus is already enabled.")
  disableAddon(myLibrary2, "stsimBurnP3Plus")
  expect_error(expect_error(enableAddon(myLibrary2, c("stsimBurnP3Plus", "stsimsf"))))

  expect_output(enableAddon(myLibrary2, "burnP3PlusCell2Fire"))
  expect_output(enableAddon(myLibrary1, "stsimBurnP3Plus"))
})

test_that("errors work", {
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(enableAddon("myLibrary"))
  expect_error(enableAddon(vector))
  expect_error(enableAddon(list))
  expect_error(enableAddon(character))
  expect_error(enableAddon(df))
  expect_error(enableAddon(library))
  expect_error(enableAddon(myLibrary1))
  expect_error(enableAddon(myScenario))
  expect_error(enableAddon(myProject))
  expect_output(enableAddon(myLibrary2, "burnP3PlusCell2Fire"))
  expect_output(enableAddon(myLibrary1, "burnp3PlusCell2Fire"))
  expect_error(enableAddon(myLibrary1, burnP3PlusCell2Fire))
  expect_error(enableAddon("burnP3PlusCell2Fire"))
})
