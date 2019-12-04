context("Testing cluster size matrix")


# things that could be tested
# The ranges of cluster sizes
# The average size is accurate
# balanced clusters are actually balanced
# Zero SD when balanced


# --------------------------
# Testing Balanced Clusters
# --------------------------
test_that("Balanced Test 1", {

  expect_equal(sd(makeClusterMat(cn = 100,avg_cs = 10, balanced = TRUE)[[1]][,2]),
               0)

})

test_that("Balanced Test 2", {

  expect_equal(sd(makeClusterMat(cn = 157,avg_cs = 12, balanced = TRUE)[[1]][,2]),
               0)

})

test_that("Balanced Test 2", {

  expect_equal(makeClusterMat(cn = 157,avg_cs = 12, balanced = TRUE)[[1]][,2],
               rep(12, 157))

})

test_that("Balanced Test 1", {

  expect_equal(makeClusterMat(cn = 100,avg_cs = 10, balanced = TRUE)[[1]][,2],
               rep(10, 100))

})


# --------------------------
# Write tests for unbalanced clusters
# --------------------------



