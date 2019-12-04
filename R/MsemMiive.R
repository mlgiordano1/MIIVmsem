#' miiv-2sls estimation for msems
#'
#' @param model_2lvl Two level SEM model syntax
#' @param data dataframe with all necessary variables
#' @param cluster character vector with name of grouping variable
#' @param (optional) sample.cov.w user supplied WITHIN clusters covariance matrix
#' @param (optional) sample.acov.w user supplied asymptotic covariance matrix for sample.cov.w
#' @param (optional) sample.cov.b user supplied BETWEEN clusters covariance matrix
#' @param (optional) sample.acov.b user supplied asymptotic covariance matrix for sample.cov.b
#' @param (optional) l1Instruments user supplied instruments for WITHIN level
#' @param (optional) l2Instruments user supplied instruments for BETWEEN level
#' @param miiv.check boolean
#' @param printProg boolean - print progress of estimation
#'
#' @return list of model fit objects
#'
#' @examples MsemMiive(model_2lvl, data, cluster)
#'
#' @export
#' @importFrom MIIVsem miivs
# ------------------------------------------------------------------------------


MsemMiive <- function(model_2lvl, data=NULL, cluster,
                       sample.cov.w=NULL, sample.acov.w = NULL,
                       sample.cov.b=NULL, sample.acov.b = NULL,
                       l1Instruments = NULL, l2Instruments=NULL,
                       miiv.check = TRUE,
                       printProg = FALSE) {
  if (printProg) {cat("--Starting input checks--\n")}
  # ---------------------
  # program some checks
  # ---------------------
  # Check #1 is did they supply either raw data OR all the sample.cov arguments
  # these are some helper vars.
  if (all(is.null(sample.cov.w),
          is.null(sample.cov.b),
          is.null(sample.acov.w),
          is.null(sample.acov.b))) {
    noCovs   <- TRUE
    allCovs  <- FALSE
    someCovs <- FALSE
  } else if (all(!is.null(sample.cov.w),
                 !is.null(sample.cov.b),
                 !is.null(sample.acov.w),
                 !is.null(sample.acov.b))) {
    noCovs   <- FALSE
    allCovs  <- TRUE
    someCovs <- FALSE
  } else {

    noCovs   <- FALSE
    allCovs  <- FALSE
    someCovs <- TRUE
  }

  # is raw data supplied
  if (!is.null(data)) {

    # raw data is supplied -- then no covs are needed
    if (any(someCovs, allCovs)) {

      stop("Raw data and 'sample.cov/sample.nobs' arguments should not be used simultaneously")

    }

  } else if (is.null(data)) {

    # raw data is not supplied --then all covs are needed
    if (someCovs) {

      stop("All sample.cov/sample.nobs arguments are necessary")

    } else if (noCovs) {

      stop("Either raw data or all 4 sample.cov/sample.nobs arguments are necessary")
    }

  }


  #Check #2 ???






  # -------------------
  # parse the model, into two levels
  # -------------------
  if (printProg) {cat("--Parse Model--\n")}
  mods <- parseMSEMSyntax(model_2lvl)



  #-------------------------------------------------------#
  # Check class of model.
  #-------------------------------------------------------#
  if (printProg) {cat("--Make Equations--\n")}
  #level 1
  if ( "miivs" == class(mods$model_level_1) ) {

    d_l1  <- mods$model_level_1$eqns
    pt_l1 <- mods$model_level_1$pt

  } else {

    res_l1 <- miivs(mods$model_level_1)
    d_l1   <- res_l1$eqns
    pt_l1  <- res_l1$pt

  }

  #level 2
  if ( "miivs" == class(mods$model_level_2) ){

    d_l2  <- mods$model_level_2$eqns
    pt_l2 <- mods$model_level_2$pt

  } else {

    res_l2 <- miivs(mods$model_level_2)
    d_l2   <- res_l2$eqns
    pt_l2  <- res_l2$pt

  }

  #-------------------------------------------------------#
  # parseInstrumentSyntax
  #-------------------------------------------------------#
  if (printProg) {cat("--Parse Instruments--\n")}
  d_l1  <- parseInstrumentSyntax(d_l1, instruments = l1Instruments, miiv.check)
  d_l2  <- parseInstrumentSyntax(d_l2, instruments = l2Instruments, miiv.check)

  #-------------------------------------------------------#
  # remove equations where there are not sufficient MIIVs
    # section taken from MIIVsem
  #-------------------------------------------------------#
  if (printProg) {cat("--Prune MIIVs--\n")}
  # level 1
  underid_l1   <- unlist(lapply(d_l1, function(eq) {
    length(eq$MIIVs) < length(eq$IVobs)
  }))
  d.un_l1 <- d_l1[underid_l1]; d_l1   <- d_l1[!underid_l1]

  # level 2
  underid_l2   <- unlist(lapply(d_l2, function(eq) {
    length(eq$MIIVs) < length(eq$IVobs)
  }))
  d.un_l2 <- d_l2[underid_l2]; d_l2   <- d_l2[!underid_l2]

#
# -----------------------------------------------
# Need to deal with exogenous vars...
# instrumented by themselves...
# current
# -----------------------------------------------
#   str(d_l1[[4]]$MIIVs)
#




  # -------------------------------------------------------------------
  # Making a list of all observed vars in the model
  #  - will be used to subset just those vars in the data or cov/acovs
  # -------------------------------------------------------------------
  if (printProg) {cat("--Subset Obs Vars--\n")}
  # all obs vars at level 1
  obsVars_w <- vector(mode = "list", length(res_l1$eqns))
  for (i in seq(obsVars_w)) {
    obsVars_w[[i]] <- c(res_l1$eqns[[i]]$DVobs, res_l1$eqns[[i]]$IVobs, res_l1$eqns[[i]]$MIIVs)
  }
  # all obs vars at level 2
  obsVars_b <- vector(mode = "list", length(res_l2$eqns))
  for (i in seq(obsVars_b)) {
    obsVars_b[[i]] <- c(res_l2$eqns[[i]]$DVobs, res_l2$eqns[[i]]$IVobs, res_l2$eqns[[i]]$MIIVs)
  }
  # combine
  obsVars_all <- unique(c(unlist(obsVars_w), unlist(obsVars_b)))
  # this will standardize the ordering or vars
  obsVars_all <- obsVars_all[order(obsVars_all)]



  # -----------------------------------------------------
  # STAGE 1 ESTIMATION: (SIGMA_W AND SIGMA_B)
  #   IF raw data is supplied
  #     estimate sigma_w, sigma_b (and associated acovs)
  #   ELSE use the supplied COVS/ACOVs
  # ----------------------------------------------------

  if (!is.null(data)) {
    if (printProg) {cat("--Stage 1 Estimation--\n")}
    # subset vars
    data <- data[,c(cluster,obsVars_all)]

    # Stage 1 estimates (sigma_w, sigma_b, acov(sigma_w) and acov(sigma_b))
    stage1 <- stage1Estimates_lavaan(data       = data,
                                     obsVars    = obsVars_all,
                                     cluster = cluster)
    # save the covs/acovs
    sample.cov.w   <- stage1$cov_within
    sample.acov.w  <- stage1$acov_within
    sample.cov.b   <- stage1$cov_between
    sample.acov.b  <- stage1$acov_between

    # muml stage 1 estimates
    # muml <- MUMLdecomp(data, cluster = cluster)
  } else {
    if (printProg) {cat("--Skipping Stage 1 Estimation (covs/acovs already input--\n")}
    # subset vars


    # function returns stage 1 ests above...
    # make them NULL here if we don't estimate
    stage1 = NULL

  }


  # -----------------------------------
  # perform estimation for each level
  # -----------------------------------
  if (printProg) {cat("--Parameter Estimation: Level 1--\n")}
  l1_results <- vector(mode = "list", length = length(d_l1))
  # loop through equation
  for (i in seq(l1_results)) {
    l1_results[[i]] <- c(thetaSeSarg(eqn_obj  = d_l1[[i]],
                                     fullCov  = sample.cov.w,
                                     fullAcov = sample.acov.w),
                         d_l1[[i]])
  }
  if (printProg) {cat("--Parameter Estimation: Level 2--\n")}
  l2_results <- vector(mode = "list", length = length(d_l2))
  # loop through each equation
  for (i in seq(l2_results)) {
    l2_results[[i]] <- c(thetaSeSarg(eqn_obj = d_l2[[i]],
                                     fullCov  = sample.cov.b,
                                     fullAcov = sample.acov.b),
                         d_l2[[i]])
  }


  if (printProg) {cat("--Done--\n")}

  return(list(stage1            = stage1,
              rawData           = data,
              level1Results     = l1_results,
              level2Results     = l2_results))
}


