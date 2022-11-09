myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession1 <- session()
mySession2 <- session()
myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package = "stsim", addon = "stsimsf")
myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld")
expected1 <- addon(myLibrary1)
expected2 <- addon(myLibrary2)
expected3 <- addon(mySession1)
expected4 <- addon(mySession2)
expected5 <- addon()

test_that("argument type", {
  expect_s3_class(expected1, "data.frame")
  expect_s4_class(myLibrary1, "SsimLibrary")
  expect_s3_class(expected2, "data.frame")
  expect_s4_class(myLibrary2, "SsimLibrary")
  expect_s3_class(expected3, "data.frame")
  expect_s3_class(expected4, "data.frame")
  expect_s3_class(expected5, "data.frame")
  expect_type(addon(myLibrary1), "list")
  expect_type(addon(myLibrary2), "list")
  expect_type(expected1$name, "character")
  expect_type(expected2$name, "logical")
})

test_that("errors", {
  expect_error(addon(x))
  expect_error(addon("myLibrary1"))
  expect_error(addon(myLibrary))
})

test_that("proper addon appears", {
  expect_equal(expected1[1,1], 'stsimsf')
  expect_equal(expected2[1,1], NA)
  expect_equal(expected3, expected4)
  expect_equal(expected3, expected5)
})


