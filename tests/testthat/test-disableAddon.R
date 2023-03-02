myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession1 <- session()
mySession2 <- session()
myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package = "burnP3Plus", addon = "burnP3PlusCell2Fire", overwrite = TRUE)
myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld", overwrite = TRUE)
myProject <- project(myLibrary1, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

test_that("can disableAddon all SsimObjects", {

  expect_message(disableAddon(myLibrary1, "burnP3PlusCell2Fire"), "Addon <burnP3PlusCell2Fire> disabled")
  expect_message(disableAddon(myLibrary1, "burnP3PlusCell2Fire"), "burnP3PlusCell2Fire is already disabled.")
  enableAddon(myLibrary1, "burnP3PlusCell2Fire")
  expect_message(disableAddon(myLibrary1, c("burnP3PlusCell2Fire")), "Addon <burnP3PlusCell2Fire> disabled")
  enableAddon(myLibrary1, "burnP3PlusCell2Fire")
  expect_error(expect_error(disableAddon(myLibrary1, c("burnP3PlusCell2Fire", "burnP3PlusPrometheus"))))
  expect_output(disableAddon((myLibrary2), "burnP3PlusCell2Fire"))
})

test_that("errors work", {
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(disableAddon("myLibrary"))
  expect_error(disableAddon(vector))
  expect_error(disableAddon(list))
  expect_error(disableAddon(character))
  expect_error(disableAddon(df))
  expect_error(disableAddon(library))
  expect_error(disableAddon(myLibrary1))
  expect_error(disableAddon(myScenario))
  expect_error(disableAddon(myProject))
  expect_error(disableAddon("burnP3PlusCell2Fire"))
  expect_output(disableAddon((myLibrary2), "burnP3PlusCell2Fire"))
})
