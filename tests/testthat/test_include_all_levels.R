context("Checks for level 1 and level 2 syntax")


#
test_that("no 'level 1' creates error", {

  expect_error(parseMSEMSyntax("Level 2: L1 =~ y1 + y2 + y3"))

})

#
test_that("no 'level 1' creates error (test 2)", {

  expect_error(parseMSEMSyntax("'level 2: L1 =~ y1 + y2 + y3"))

})

#
test_that("no 'level 2' creates error", {

  expect_error(parseMSEMSyntax("level 1: L1 =~ y1 + y2 + y3"))

})

#
test_that("no 'level 2' creates error", {

  expect_error(parseMSEMSyntax("level 1: L1 =~ y1 + y2 + y3"))

})
