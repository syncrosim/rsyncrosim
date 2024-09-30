### ApexRMS
### 2024-09-27
### Below script tests the following functions:
### * addRow

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
myLibraryName1 <- file.path(tempdir(), "testlib")
myLibraryName2 <- file.path(tempdir(), "mylib")
mySession <- session("C:/Program Files/SyncroSim Studio")

# Tests ----

# generate dataframes, vectors, and lists
multipleRows <- data.frame(mpg = c(40, 50, 75),
                           wt = c(4, 7, 6))

wrongcolumnnames <- data.frame(x = c(1, 2, 3),
                               y = c(31, 50, 67))

vector <- c(10, 20, 30)

# vector2 <- c(mpg = 1, cyl = 2, disp = 3)
# vector3 <- c(mpg = 1, cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11) # vectors don't work

nozero <- data.frame(mpg = 1,
                     cyl = 2,
                     disp = 3,
                     hp = 4,
                     drat = 5,
                     wt = 6,
                     qsec = 7,
                     vs = 8,
                     am = 9,
                     gear = 10,
                     carb = 11)

undefine <- data.frame(matrix(0,
                              nrow = 1,
                              ncol = 12))

chardf <- data.frame(x = c("one", "two", "three", "four", "five"),
                     y = c("six", "seven", "eight", "nine", "ten"),
                     z = c("eleven", "twelve", "thirteen", "fourteen", "fifteen"))

charl <- list(x = c("one", "two", "three", "four", "five"),
              y = c("six", "seven", "eight", "nine", "ten"),
              z = c("eleven", "twelve", "thirteen", "fourteen", "fifteen"))

charold <- data.frame(mpg = "one",
                      cyl = "two",
                      disp = "three",
                      hp = "four",
                      drat = "five",
                      wt = "six",
                      qsec = "seven",
                      vs = "eight",
                      am = "nine",
                      gear = "ten",
                      carb = "eleven")

charoldl <- list(c(mpg = "one",
                   cyl = "two",
                   disp = "three",
                   hp = "four",
                   drat = "five",
                   wt = "six",
                   qsec = "seven",
                   vs = "eight",
                   am = "nine",
                   gear = "ten",
                   carb = "eleven"))

charoldf <- as.factor(list(c(mpg = "one",
                             cyl = "two",
                             disp = "three",
                             hp = "four",
                             drat = "five",
                             wt = "six",
                             qsec = "seven",
                             vs = "eight",
                             am = "nine",
                             gear = "ten",
                             carb = "eleven")))

# oldDataframe <- as.data.frame(mtcars)
# newDataframe1 <- addRow(oldDataframe, list(mpg = 100, wt = 10))
# newDataframe2 <- addRow((as.data.frame(mtcars)), multipleRows)
# newDataframe3 <- addRow((as.data.frame(mtcars)), nozero)
# newDataframe4 <- addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen"))
# newDataframe5 <- addRow(oldDataframe, vector3)

# test that all argument types work
test_that("argument types all work", {
  expect_s3_class(addRow((as.data.frame(mtcars)), list(mpg = 100, wt = 10)), "data.frame") # vectors don't work
  expect_s3_class(addRow((as.data.frame(mtcars)), multipleRows), "data.frame")
  expect_s3_class(addRow((as.data.frame(mtcars)), nozero), "data.frame")
  expect_s3_class(addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen")), "data.frame")
})

# test that 
test_that("correct dataframe size", {
  expect_length(addRow((as.data.frame(mtcars)), list(mpg = 100, wt = 10)), 11)
  expect_length(addRow((as.data.frame(mtcars)), multipleRows), 11)
  expect_length(addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen")), 3)
  expect_equal(nrow(as.data.frame(mtcars)), 32)
  expect_equal(nrow(addRow((as.data.frame(mtcars)), list(mpg = 100, wt = 10))), 33)
  expect_equal(nrow(addRow((as.data.frame(mtcars)), nozero)), 33)
  expect_equal(nrow(addRow((as.data.frame(mtcars)), multipleRows)), 35)
  expect_equal(nrow(addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen"))), 6)
  expect_equal(dim(addRow((as.data.frame(mtcars)), list(mpg = 100, wt = 10))), c(33, 11))
  expect_equal(dim(addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen"))), c(6, 3))
})


test_that("number NAs", {
  expect_equal(sum(is.na(addRow(as.data.frame(mtcars), list(mpg = 100, wt = 10)))), 9)
  expect_equal(sum(is.na(addRow((as.data.frame(mtcars)), multipleRows))), 27)
  expect_equal(sum(is.na(multipleRows)), 0)
  expect_equal(sum(is.na(as.data.frame(mtcars))), 0)
  expect_equal(sum(is.na(addRow((as.data.frame(mtcars)), nozero))), 0)
  expect_equal(sum(is.na(addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen")))), 0)
})


test_that("errors work", {
  expect_error(addRow(as.data.frame(mtcars), wrongcolumnnames))
  expect_error(addRow(as.data.frame(mtcars), vector))
  expect_error(addRow(as.data.frame(mtcars), list = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
  expect_error(addRow(as.data.frame(mtcars), undefine))
  expect_error(addRow(as.data.frame(mtcars), 1))
  expect_error(addRow(as.data.frame(mtcars), "ten"))
  expect_error(addRow(charl, addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen"))))
  expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(expect_warning(addRow((as.data.frame(mtcars)), charold))))))))))))
  expect_error(addRow(as.data.frame(mtcars), charoldl))
  expect_error(addRow(as.data.frame(mtcars), charoldf))
  expect_error(addRow(charl, as.data.frame(mtcars)))
  expect_error(addRow(vector, as.data.frame(mtcars)))
  expect_error(addRow(charoldf, as.data.frame(mtcars)))
})

# can't reproduce error "Invalid values for ", cName, " : "