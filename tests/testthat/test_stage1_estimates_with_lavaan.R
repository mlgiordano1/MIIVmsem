context("Testing Stage 1 Estimates with Lavaan")



# I noticed with the varname "y10" I was getting problems

# test 1 - this should work correctly already




mydata  <- readRDS(system.file("testdata/testData.rds", package = "MSEMMIIVSEM"))
results <- readRDS(system.file("testdata/testResults.rds", package = "MSEMMIIVSEM"))

test_that("Stage 1 Estimate are Correct", {

  expect_identical(stage1Estimates_lavaan(data=mydata, obsVars = paste0("y", 1:9), clusterVar = "cluster"),
                 results)
})


