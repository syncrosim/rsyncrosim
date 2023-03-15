#need to test prompts

test_that("package added", {
  mySession1 <- session()
  mySession2 <- session()
  addPackage("helloworldSpatial", session = mySession1)
  myLibrary1 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim", session = mySession1)
  addPackage("stsim", session = mySession2)
  myLibrary2 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim", session = mySession2)

  expect_equal(removePackage(session = mySession1, name = "helloworldSpatial", force = TRUE), TRUE)
  expect_equal(removePackage(session = mySession2, name = "stsim", force = TRUE), TRUE)
  expect_error(removePackage(session = mySession2, name = "stsim", force = TRUE))
  addPackage("stsim", session = mySession2)
  expect_equal(removePackage(name = "stsim", force = TRUE), TRUE)
})

test_that("test errors", {
  mySession1 <- session()
  mySession2 <- session()
  addPackage("helloworldSpatial", session = mySession1)
  myLibrary1 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim", session = mySession1)
  addPackage("stsim", session = mySession2)
  myLibrary2 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim", session = mySession2)

  expect_error(removePackage(session = mySession1))
  expect_error(removePackage(session = "mySession1"))
  expect_error(removePackage(name = helloworld, session = mySession2))
  expect_error(removePackage(name = "helloworldSpatial", session = "mySession1"))
  removePackage("helloworldSpatial", session = mySession2, force = TRUE)
  expect_error(expect_equal(removePackage(session = mySession2, name = "helloworldSpatial"), TRUE))

})
