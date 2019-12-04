context("Test model parsing")



# -------------------------
# First set of tests
# -------------------------
twoLevelModel <- "
Level 2:
l1 =~ y1 + y2 + y3
l2 =~ y4 + y5
Level 1:
l1a =~ y1a + y2a + y3a
l2a =~ y4a + y5a
"
l1Model <- "
l1a =~ y1a + y2a + y3a
l2a =~ y4a + y5a
"

l2Model <- "
l1 =~ y1 + y2 + y3
l2 =~ y4 + y5
"

test_that("Parsed L1 works", {

  expect_equal(lavaan::lavaanify(l1Model),
               lavaan::lavaanify(parseMSEMSyntax(twoLevelModel)$model_level_1))

})

test_that("Parsed L2 works", {

  expect_equal(lavaan::lavaanify(l2Model),
               lavaan::lavaanify(parseMSEMSyntax(twoLevelModel)$model_level_2))

})


# -------------------------
# Second set of tests
# -------------------------
twoLevelModel <- "
Level 1:
l1a =~ y1a + y2a + y3a
l2a =~ y4a + y5a

level 2:
L1b =~ y1 + y2 + y3
L2b =~ y4 + y5

"
l1Model <- "
l1a =~ y1a + y2a + y3a
l2a =~ y4a + y5a
"

l2Model <- "
L1b =~ y1 + y2 + y3
L2b =~ y4 + y5
"

test_that("Parsed L1 works", {

  expect_equal(lavaan::lavaanify(l1Model),
               lavaan::lavaanify(parseMSEMSyntax(twoLevelModel)$model_level_1))

})

test_that("Parsed L2 works", {

  expect_equal(lavaan::lavaanify(l2Model),
               lavaan::lavaanify(parseMSEMSyntax(twoLevelModel)$model_level_2))

})





