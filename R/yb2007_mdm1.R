#' helper function, which takes beta (vector of all estimates) and converts
#' mu vector, sigw matrix, and sigb matrix;
#'
#' @param p scalar value. # of variables
#' @param beta vector of all estimates in order from mu, sigw, sigb
#'
#' @return list
#'
#' @examples yb2007_mdm1(5, beta)
#'
#' @export
# ------------------------------------------------------------------------------



yb2007_mdm1 <- function(p, beta, varNames) {
  # parameters given back to mu and sig;
  mu   <- beta[1:p,]


  # -----------
  # make sigw
  # -----------
  sigw <- matrix(nrow = p,ncol = p, 0)
  nc   <- p

  for (i in 1:p) {
    for (j in i:p) {
      nc=nc+1;
      sigw[j,i] <- beta[nc]
      sigw[i,j] <- beta[nc]
    }
  }
  # name rows and columns
  colnames(sigw) <- varNames
  rownames(sigw) <- varNames
  # vectorize
  vsigw <- vech(sigw)



  # ------------------------------------------
  # make sigb
  #    * note we do not reset nc (the counter)
  # ------------------------------------------
  sigb <- matrix(nrow = p, ncol = p,0)
  for (i in 1:p) {
    for (j in i:p) {
      nc <- nc + 1
      sigb[j,i] <- beta[nc]
      sigb[i,j] <- beta[nc]
    }
  }
  # name rows and columns
  colnames(sigb) <- varNames
  rownames(sigb) <- varNames
  vsigb <- vech(sigb)




  return(list(mu    = mu,
              sigb  = sigb,
              vsigb = vsigb,
              sigw  = sigw,
              vsigw = vsigw))

}
