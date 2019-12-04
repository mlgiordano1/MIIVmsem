context("Testing process2cov")



# I noticed with the varname "y10" I was getting problems

# test 1 - this should work correctly already

m <- MIIVsem:::revVech(x = 1:15)
rownames(m) <- paste0("y", 1:5)
colnames(m) <- paste0("y", 1:5)


obj <- processCov2(covMatrix = m, dv = "y2", si = "y1", instru   = c("y3", "y4", "y5"))


test_that("SVV is correct", {

  expect_equal(obj$svvIndex, 10:15)

})


# this same test with y5-y10 creates problems
m <- MIIVsem:::revVech(x = 1:15)
rownames(m) <- paste0("y", 6:10)
colnames(m) <- paste0("y", 6:10)

obj <- processCov2(covMatrix = m, dv = "y7", si = "y6", instru   = c("y8", "y9", "y10"))

test_that("SVV is correct2", {

  expect_equal(obj$svvIndex, 10:15)

})


# Crazy var names
m <- MIIVsem:::revVech(x = 1:15)
rownames(m) <- c("Tracy", "Stacy", "Stan1", "Stan2", "Stan3")
colnames(m) <- c("Tracy", "Stacy", "Stan1", "Stan2", "Stan3")

obj <- processCov2(covMatrix = m, dv = "Stacy", si = "Tracy", instru   = c("Stan1", "Stan2", "Stan3"))

test_that("SVV is correct2", {

  expect_equal(obj$svvIndex, 10:15)

})









