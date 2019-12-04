#' All est steps together
#'
#'
#' @param cov vectorized covariance matrix
#' @param varNames names of all variables (in order) from vechCov
#' @param dv dependent var
#' @param si scaling indicator
#' @param instru instument set
#' @param acov associated asymptotic covariance matrix for cov
#'
#' @return estimates
#'
#' @examples thetaSeSarg(vechCov, varNames, dv, si, instru)
#'
#' @export
#' @importFrom numDeriv jacobian
# ------------------------------------------------------------------------------


thetaSeSarg <- function(eqn_obj, fullCov, fullAcov) {

  # y = dv
  # z = independent var (scaling indicators)
  # v = instruments
  # covNames <- rownames(fullCov)

  # ---------------------------------------------
  # step 1 re-order the fullCov and the fullAcov
  # ---------------------------------------------
  fullCov <- fullCov[unique(c(eqn_obj$DVobs, eqn_obj$IVobs, eqn_obj$MIIVs)),
                     unique(c(eqn_obj$DVobs, eqn_obj$IVobs, eqn_obj$MIIVs))]

  # this is a little redic, but this is how I'm reordering ACOV
  acovNames <- paste("\\b", getCovNames(varList = rownames(fullCov))$namesForCols, "\\b|\\b",
                     getCovNames(varList = rownames(fullCov))$namesForRows, "\\b",
                     sep = "")
  temp <- vector(mode = "numeric", length = length(acovNames))
  for (i in seq(acovNames)) {
    temp[i] <- grep(acovNames[[i]], x = rownames(fullAcov))
  }
  fullAcov_reordered <- fullAcov[temp,temp]



  vechCov <- vech(fullCov)


  # -----------------------
  # make the theta function
  # Have to do this here to pull through the indices?
  # When I had the indices as arguments, things weren't working
  # -----------------------
  theta <- function(x) {

    m <- revVech(x)

    # Svv - (instrument covariance matrix)
    Svv <- m[(rownames(fullCov) %in% eqn_obj$MIIVs),
             (rownames(fullCov) %in% eqn_obj$MIIVs)]

    # Svz - instr with scaling indicator
    Svz <- m[(rownames(fullCov) %in% eqn_obj$MIIVs),
             (rownames(fullCov) %in% eqn_obj$IVobs)]

    # Svy instrument with DV
    Svy <- m[(rownames(fullCov) %in% eqn_obj$MIIVs),
             (rownames(fullCov) %in% eqn_obj$DVobs)]


    # beta coefs (Fox, 1979)
    solve(t(Svz) %*% solve(Svv) %*% Svz) %*% (t(Svz) %*% solve(Svv) %*% Svy)
  }



  # --------------
  # 2SLS estimates
  # --------------
  est <- theta(x = c(vechCov))



  # --------------------------
  # correcting standard errors
  # --------------------------
  acov <- fullAcov_reordered
  # j matrix
  J <- jacobian(func = theta, x = vechCov)
  # correcting acov
  ACOV_CORRECTED <- J %*% acov %*% t(J)
  se <- sqrt(diag(ACOV_CORRECTED))





  newSarg <- sarg_jc2018(eqn_obj = eqn_obj,
                         vCov = vechCov,
                         acov = acov,
                         fullCov = fullCov,
                         Svv     = fullCov[(rownames(fullCov) %in% eqn_obj$MIIVs),
                                           (rownames(fullCov) %in% eqn_obj$MIIVs)],
                         Svz     = fullCov[(rownames(fullCov) %in% eqn_obj$MIIVs),
                                           (rownames(fullCov) %in% eqn_obj$IVobs)],
                         Svy     = fullCov[(rownames(fullCov) %in% eqn_obj$MIIVs),
                                           (rownames(fullCov) %in% eqn_obj$DVobs)])







  return(list(est=est, se = se, newSarg = newSarg))
}

