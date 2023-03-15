test_that("can retrive printCmd from Session", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(printCmd(mySession), "logical")
  expect_error(expect_error(printCmd(mySession)))
  expect_error(expect_error(printCmd()))
  expect_equal(printCmd(mySession), printCmd())

})

test_that("errors work", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  character <- "character"

  #expect_warning(expect_warning((printCmd("mySession")))) expect warning function doesn't work for Ctrl + Shift + T
  #expect_warning(expect_warning(printCmd("character")))
  expect_error(printCmd(c(1,2,3)))
  expect_error(printCmd(list(1,2,3)))
  #expect_warning(expect_warning(printCmd(character)))
  expect_error(printCmd(data.frame(list(x = 1, y = 2, z  = 3))))
  expect_error(printCmd(Session))
  expect_error(printCmd(myProject))
  expect_error(printCmd(myLibrary))
  expect_error(printCmd(myScenario))
})
