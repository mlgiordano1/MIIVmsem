context("Listing level more than once creates error")


model1 <- "
Level 2:
l1 =~ y1 + y2 + y3
l2 =~ y4 + y5
Level 1:
l1 =~ y1 + y2 + y3
l2 =~ y4 + y5
Level 2:
l3 =~ y6 + y7 + y8
"

test_that("Level 1 included twice", {

  expect_error(parseMSEMSyntax(model1))

})



