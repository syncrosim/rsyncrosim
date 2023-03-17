
test_that("argument type", {
  myLibraryName1 <- file.path(tempdir(),"testlib")
  myLibraryName2 <- file.path(tempdir(),"mylib")
  mySession1 <- session()
  mySession2 <- session()
  myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package = "burnP3Plus", addon = "burnP3PlusCell2Fire", overwrite = TRUE)
  myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld", overwrite = TRUE)

  expect_s3_class(addon(myLibrary1), "data.frame")
  expect_s4_class(myLibrary1, "SsimLibrary")
  expect_s3_class(addon(myLibrary2), "data.frame")
  expect_s4_class(myLibrary2, "SsimLibrary")
  expect_s3_class(addon(mySession1), "data.frame")
  expect_s3_class(addon(mySession2), "data.frame")
  expect_s3_class(addon(), "data.frame")
  expect_type(addon(myLibrary1), "list")
  expect_type(addon(myLibrary2), "list")
  expect_type((addon(myLibrary1))$name, "character")
  expect_type((addon(myLibrary2))$name, "logical")
})

test_that("errors", {
  myLibraryName1 <- file.path(tempdir(),"testlib")
  myLibraryName2 <- file.path(tempdir(),"mylib")
  mySession1 <- session()
  mySession2 <- session()
  myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package = "burnP3Plus", addon = "burnP3PlusCell2Fire", overwrite = TRUE)
  myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld", overwrite = TRUE)

  expect_error(addon(x))
  expect_error(addon("myLibrary1"))
  expect_error(addon("burnP3PlusCell2Fire"))
})

test_that("proper addon appears", {
  myLibraryName1 <- file.path(tempdir(),"testlib")
  myLibraryName2 <- file.path(tempdir(),"mylib")
  mySession1 <- session()
  mySession2 <- session()
  myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession1, package = "burnP3Plus", addon = "burnP3PlusCell2Fire", overwrite = TRUE)
  myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession2, package = "helloworld", overwrite = TRUE)

  expect_equal((addon(myLibrary1))[1,1], 'burnP3PlusCell2Fire')
  expect_equal((addon(myLibrary2))[1,1], NA)
  expect_equal((addon(mySession1)), (addon(mySession2)))
  expect_equal((addon(mySession1)), (addon()))
})


