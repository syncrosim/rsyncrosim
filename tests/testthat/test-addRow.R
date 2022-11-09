multipleRows <- data.frame(mpg = c(40, 50, 75), wt = c(4, 7, 6))
wrongcolumnnames <- data.frame(x = c(1, 2, 3), y = c(31, 50, 67))
vector <- c(10, 20, 30)
#vector2 <- c(mpg = 1, cyl = 2, disp = 3)
#vector3 <- c(mpg = 1, cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11) # vectors don't work
nozero <- data.frame(mpg = 1, cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11)
undefine <- data.frame(matrix(0, nrow=1, ncol=12))
chardf <- data.frame(x = c("one", "two", "three", "four", "five"), y = c("six", "seven", "eight", "nine", "ten"), z = c("eleven", "twelve", "thirteen", "fourteen", "fifteen"))
charl <- list(x = c("one", "two", "three", "four", "five"), y = c("six", "seven", "eight", "nine", "ten"), z = c("eleven", "twelve", "thirteen", "fourteen", "fifteen"))
charold <- data.frame(mpg = "one", cyl = "two", disp = "three", hp = "four", drat = "five", wt = "six", qsec = "seven", vs = "eight", am = "nine", gear = "ten", carb = "eleven")
charoldl <- list(c(mpg = "one", cyl = "two", disp = "three", hp = "four", drat = "five", wt = "six", qsec = "seven", vs = "eight", am = "nine", gear = "ten", carb = "eleven"))
charoldlw <- list(mpg = "one", cyl = "two", disp = "three", hp = "four", drat = "five", wt = "six", qsec = "seven", vs = "eight", am = "nine", gear = "ten", carb = "eleven")
charoldf <- as.factor(list(c(mpg = "one", cyl = "two", disp = "three", hp = "four", drat = "five", wt = "six", qsec = "seven", vs = "eight", am = "nine", gear = "ten", carb = "eleven")))

oldDataframe <- as.data.frame(mtcars)
newDataframe1 <- addRow(oldDataframe, list(mpg = 100, wt = 10))
newDataframe2 <- addRow(oldDataframe, multipleRows)
newDataframe3 <- addRow(oldDataframe, nozero)
newDataframe4 <- addRow(chardf, list(x = "sixteen", y = "seventeen", z = "eigteen"))
#newDataframe5 <- addRow(oldDataframe, vector3)

test_that("argument types all work", {
  expect_s3_class(newDataframe1, "data.frame") #vectors don't work
  expect_s3_class(newDataframe2, "data.frame")
  expect_s3_class(newDataframe3, "data.frame")
  expect_s3_class(newDataframe4, "data.frame")
})
test_that("correct dataframe size", {
  expect_length(newDataframe1, 11)
  expect_length(newDataframe2, 11)
  expect_length(newDataframe4, 3)
  expect_equal(nrow(oldDataframe), 32)
  expect_equal(nrow(newDataframe1), 33)
  expect_equal(nrow(newDataframe3), 33)
  expect_equal(nrow(newDataframe2), 35)
  expect_equal(nrow(newDataframe4), 6)
  expect_equal(dim(newDataframe1), c(33,11))
  expect_equal(dim(newDataframe4), c(6,3))
})
test_that("number NAs", {
  expect_equal(sum(is.na(newDataframe1)), 9)
  expect_equal(sum(is.na(newDataframe2)), 27)
  expect_equal(sum(is.na(multipleRows)), 0)
  expect_equal(sum(is.na(oldDataframe)), 0)
  expect_equal(sum(is.na(newDataframe3)), 0)
  expect_equal(sum(is.na(newDataframe4)), 0)
})
test_that("errors work", {
  expect_error(addRow(oldDataframe, wrongcolumnnames))
  expect_error(addRow(oldDataframe, vector))
  expect_error(addRow(oldDataframe, list = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
  expect_error(addRow(oldDataframe, undefine))
  expect_error(addRow(oldDataframe, 1))
  expect_error(addRow(oldDataframe, "ten"))
  expect_error(addRow(charl, newDataframe4))
  #expect_warning(addRow(oldDataframe, charold)) #gives me warnings but don't register as such
  #expect_warning(addRow(oldDataframe, charoldlw))
  expect_error(addRow(oldDataframe, charoldl))
  expect_error(addRow(oldDataframe, charoldf))
  expect_error(addRow(charl, oldDataframe))
  expect_error(addRow(vector, oldDataframe))
  expect_error(addRow(charoldf, oldDataframe))
})

# can't reproduce error "Invalid values for ", cName, " : "


