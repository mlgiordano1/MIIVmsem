#' Obtains stage 1 estimates with lavaan
#'
#' @param data dataframe
#' @param obsVars names of the observed variables
#' @param clusterVar name of the cluster variable
#'
#' @return covariance matrices and the related acov matrices
#'
#' @examples stage1Estimates_lavaan(data, obsVars, clusterVar)
#'
#' @export
#' @importFrom lavaan sem
#' @importFrom lavaan parametertable
#' @importFrom lavaan vcov
# ------------------------------------------------------------------------------



# --------------------------
# this to test the code with - delete once done
# --------------------------
# try(setwd("C:/users/Michael/google drive/psyc/research/dissertation/programming/20191001 - checking Mplus Data Generation"))
# try(setwd("C:/users/mgiordan/google drive/psyc/research/dissertation/programming/20191001 - checking Mplus Data Generation"))
#
# allData <- readRDS(file = "./data/alldf.rds")
#
# myData <- allData[[75]]
# names(myData) <- c(paste0("y", 1:9), "cluster")
# clusterVar <- "cluster"
#
#
# variables <- paste0("y", 1:9)
# obsVars <- variables
# data = myData

stage1Estimates_lavaan <- function(data, obsVars, clusterVar) {
  # -----------------------------
  # add necessary checks here
  # -----------------------------


  # ----------------------------
  # write lavaan model
  # ----------------------------
  mod <- writeSatLavModel(obsVars = obsVars)


  # ----------------------------
  # fit with lavaan
  # ----------------------------
  fit <- sem(model = mod, data = data, cluster = clusterVar)

  # ------------------------
  # pull out the estimates
  # ------------------------
  covs <- parametertable(fit)
  p <- length(obsVars)
  # unique elements in the cov matrix
  ps <- (p*(p+1))/2
  # right now the order of the paramter table is
  # all level 1 ests, intercepts and then all level 2
  # should write tests in case this changes
  cov_within <- revVech(covs$est[1:ps])
  colnames(cov_within) <- rownames(cov_within) <- obsVars

  cov_between <- revVech(covs$est[(ps+p+1):(ps+p+ps)])
  colnames(cov_between) <- rownames(cov_between) <- obsVars

  # -------------------
  # pull out the Asymptotic Var/Covars or the Elements of the covariance matrices!
  # -------------------
  # variance/covariance matrix of estimates from lavaan
  acovs <- vcov(fit)
  # this is the names used by lavaan
  names_columns <- getCovNames(obsVars)$namesForCols
  names_rows    <- getCovNames(obsVars)$namesForRows
  # pull out the within and between elements by name
  # NOTE lavaan uses the column names
  acov_within  <- acovs[names_columns, names_columns]
  acov_between <- acovs[paste0(names_columns, ".l2"), paste0(names_columns, ".l2")]
  # renaming the rows
  rownames(acov_within) <- rownames(acov_between) <- names_rows

  return(list(cov_within   = cov_within,
              acov_within  = acov_within,
              cov_between  = cov_between,
              acov_between = acov_between
              ))

}
