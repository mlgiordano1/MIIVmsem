#' Corrected Sargan's test from Jin and Cao (2018).
#'
#'
#' @param vechCov vectorized covariance matrix

#'
#' @return list of sarg est, df, and p
#'
#' @examples sarg_jc2018(vechCov, varNames, dv, si, instru)
#'
#' @export
#' @importFrom numDeriv jacobian
#' @importFrom expm sqrtm
#' @importFrom MASS ginv
# ------------------------------------------------------------------------------

sarg_jc2018 <- function(eqn_obj, fullCov, vCov, acov, Svv, Svz, Svy) {
  # notes
  # v = instruments (instru)
  # y = dependent var (dv)
  # z = scaling indicator (si)

  # -----------------------------------------------------
  # this is a function so I can take partial deriviatives
  # -----------------------------------------------------
  theta <- function(x) {

    m <- revVech(x)

    # Svv - (instrument covariance matrix)
    Svv <- m[(rownames(fullCov) %in% eqn_obj$MIIVs), (rownames(fullCov) %in% eqn_obj$MIIVs)]
    # Svz - instr with scaling indicator
    Svz <- m[(rownames(fullCov) %in% eqn_obj$MIIVs), (rownames(fullCov) %in% eqn_obj$IVobs)]
        # Svy instrument with DV
    Svy <- m[(rownames(fullCov) %in% eqn_obj$MIIVs), (rownames(fullCov) %in% eqn_obj$DVobs)]


    # beta coefs (Fox, 1979)
    est <- solve(t(Svz) %*% solve(Svv) %*% Svz) %*% (t(Svz) %*% solve(Svv) %*% Svy)

    Svy - (Svz %*% est)
  }



  # j matrix
  J <- jacobian(func = theta, x = vCov)



  # ------------------------------------
  # final computations
  # ------------------------------------
  omega <- J %*% acov %*% t(J)

  C <- solve(omega,  tol = 1e-25)
  C12 <- sqrtm(C)

  # make Q (need the dimensions of Q for diag)
  Q <- diag(nrow(omega)) - C12 %*% Svz %*% solve(t(Svz) %*% solve(Svv) %*% Svz) %*% t(Svz) %*% solve(Svv) %*% omega %*% C12

  G <- ginv( Q %*% t(Q) )

  sarg <- t(theta(x = vCov)) %*% C12 %*% G %*% C12 %*% (theta(x = vCov))


}
