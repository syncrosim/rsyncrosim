myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession1 <- session()
mySession2 <- session()

removePackage(session = mySession1, name = "burnP3Plus")
myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package= "burnP3Plus")
addPackage("burnP3Plus", session = mySession1, force = TRUE)

myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld")


test_that("package added", {
  expect_equal(removePackage(session = mySession1, name = "burnP3Plus"), TRUE)
  expect_equal(removePackage(session = mySession2, name = "helloworld"), FALSE)
  expect_equal(removePackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg"), TRUE)
  expect_equal(removePackage(name = "burnP3Plus"), FALSE)
})

test_that("test errors", {
  expect_error(removePackage(session = mySession1))
  expect_error(removePackage(session = "mySession1"))
  expect_error(removePackage(name = "helloworld", session = "mySession1"))
  #expect_message(removePackage(name = "test"), "The package 'test' was not found in the package index.")
  expect_message(removePackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg"), "Cannot find file: C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg")
  expect_error(expect_equal(removePackage(session = mySession2, name = "helloworld"), TRUE))
})

#capture.output(removePackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg"))
#value((removePackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg"))) #uses future package
#expect_output((removePackage(session = mySession2, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg")), "Package installed from file <", name, ">")
