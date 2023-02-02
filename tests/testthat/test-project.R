test_that("can retrive project", {
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")

  #project argument
  expect_error(expect_error(project(myLibrary, project = "New Project")))
  expect_error(expect_error(project(myLibrary, project = "Definitions")))
  myProject1 <- project(myLibrary, project = "New Project")
  myProject2 <- project(myLibrary, project = "Definitions")
  expect_error(expect_error(project(myLibrary, project = 1)))
  expect_error(expect_error(project(myLibrary, project = c(1,44))))
  #expect_error(expect_error(project(myLibrary, project = myProject1))) #Project object

  #ssimObject arguments
  myScenario <- scenario(myProject1, scenario = "My Scenario")
  expect_error(expect_error(project(myLibrary)))
  expect_equal(project(myLibrary), project("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim"))
  expect_error(expect_error(project(myScenario)))
  expect_error(expect_error(project(myProject1)))

  #class and type
  expect_type(myProject1, "S4")
  expect_s4_class(myProject1, "Project")
  expect_type(myProject2, "S4")
  expect_s4_class(myProject2, "Project")

  #sourceProject argument
  expect_error(expect_error(project(myLibrary, project = "Another Project", sourceProject = 1, overwrite = TRUE)))
  expect_error(expect_error(project(myLibrary, project = "More Project", sourceProject = 44, overwrite = TRUE)))

  #summary argument
  expect_equal(project(myLibrary), project(myLibrary, summary = TRUE))
  expect_equal(project(myProject1), project(myProject1, summary = FALSE))
  expect_equal(project(myLibrary, project = "New Project"), project(myProject1, summary = FALSE))
  expect_s3_class(project(myLibrary), "data.frame")

  #forceElements argument
  expect_equal(project(myLibrary, summary = TRUE), project(myLibrary, summary = TRUE, forceElements = TRUE))
  expect_equal(project(myLibrary, summary = FALSE), project(myLibrary, summary = FALSE, forceElements = FALSE))
  expect_type(project(myLibrary, forceElements = TRUE), "list")

  #overwrite argument
  expect_equal(nrow(project(myLibrary, forceElements = TRUE)), 6)
  project(myLibrary, project = "overwrite")
  expect_equal(nrow(project(myLibrary, forceElements = TRUE)), 7)
  project(myLibrary, project = "overwrite", overwrite = TRUE)
  expect_equal(nrow(project(myLibrary, forceElements = TRUE)), 6)
})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(),"empty")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject1 <- project(myLibrary, project = "New Project")
  myProject2 <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject1, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(project("myLibrary"))
  expect_error(project(vector))
  expect_error(project(list))
  expect_error(project(character))
  expect_error(project(df))
  expect_error(project(library))
  expect_error(project(C(myProject1, myProject2)))
  expect_error(project(mySession))
  expect_error(project(myLibrary, project = "Another Project", sourceProject = 32))
  expect_error(project(myLibrary, project = Project1))

})
