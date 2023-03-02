callbackFunction <- function(x, iteration, timestep) {
  print(paste0("Breakpoint hit: ", scenarioId(x)))
}
addPackage("helloworldSpatial")
myLibraryName <- file.path(tempdir(),"testlibbreak")
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         package = "helloworldSpatial")
myScenario1 <- scenario(myLibrary, "My Scenario1")
myScenario2 <- scenario(myLibrary, "My Scenario2")
myScenario1 <- addBreakpoint(x= myScenario1,
                            transformerName= "helloworldSpatial_Primary",
                            breakpointType = "bi",
                            arguments = c(1,2),
                            callback = callbackFunction)
myScenario2 <- addBreakpoint(x= myScenario2,
                             transformerName= "helloworldSpatial_Primary",
                             breakpointType = "bi",
                             arguments = c(1,2),
                             callback = callbackFunction)
myScenario2 <- deleteBreakpoint(myScenario2)
expected <- capture.output(breakpoint(myScenario1))

test_that("output works", {
  expect_output(breakpoint(myScenario1))
  expect_equal(expected[1], "Transfomer name: helloworldSpatial_Primary")
  expect_equal(expected[3], "Arguments:       1,2")
})

test_that("errors works", {
  expect_error(expect_output(breakpoint(myScenario2)))
  expect_error(breakpoint(myLibrary))
  expect_error(breakpoint(mySession))
  expect_error(breakpoint("myScenario1"))
  expect_error(breakpoint(c(1,2,3)))
  expect_error(breakpoint(list = c(1,2,3)))
  expect_error(breakpoint(data.frame(c(1,2,3))))
})
