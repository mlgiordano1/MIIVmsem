library(MIIVmsem)

# read in the data
dat <- read.table("C:/Users/mgiordan/Google Drive/PSYC/RESEARCH/dissertation/programming/miivmsem/tests/famIQData.dat")
names(dat) <- c("fam", "id", paste0("y", 1:6))



model <- "
Level: 1
l1w =~ y1 + y2 + y3
l2w =~ y4 + y5 + y6
l1w ~~ l2w

Level: 2
l1b =~ y1 + y2 + y3
l2b =~ y4 + y5 + y6
l1b ~~ l2b
"

fit <- MsemMiive(model_2lvl = model,
                 data       = dat,
                 cluster    = "fam",
                 printProg  = TRUE)

parameterEstimatesTable(fit)
