myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession1 <- session()
mySession2 <- session()

addPackage(session = mySession1, name = "burnP3Plus")
myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package= "burnP3Plus")
removePackage("burnP3Plus", session = mySession1, force = TRUE)

myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld")


test_that("package added", {
  expect_equal(addPackage(session = mySession1, name = "burnP3Plus"), TRUE)
  expect_equal(addPackage(session = mySession2, name = "helloworld"), FALSE)
  expect_equal(addPackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg"), TRUE)
  expect_equal(addPackage(name = "burnP3Plus"), FALSE)
})

test_that("test errors", {
  expect_error(addPackage(session = mySession1))
  expect_error(addPackage(session = "mySession1"))
  expect_error(addPackage(name = "helloworld", session = "mySession1"))
  #expect_message(addPackage(name = "test"), "The package 'test' was not found in the package index.")
  expect_message(addPackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg"), "Cannot find file: C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg")
  expect_error(expect_equal(addPackage(session = mySession2, name = "helloworld"), TRUE))
})

#capture.output(addPackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg"))
#value((addPackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg"))) #uses future package
#expect_output((addPackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg")), "Package installed from file <", name, ">")
