#' creates a permutation matrix so that the order of elements in the Gamma matrix are rearranged
#'
#'
#' @param p description tbd
#'
#' @return matrix
#'
#' @examples yb2007_permuteGamma(6)
#'
#' @export
# ------------------------------------------------------------------------------




yb2007_permuteGamma <- function(p) {
  ps=p*(p+1)/2

  amat = matrix(nrow = p, ncol = p, 0)
  bmat = matrix(nrow = p, ncol = p, 0)

  # -----------------------
  # filling in amat
  # -----------------------
  na   = 0
  for (j in 1:p) {

    for (i in j:p) {

      na = na+1

      amat[i,j] = na

    }
  }
  va <- vech(amat)

  # -----------------------
  # filling in amat
  # -----------------------
  nb=0
  for (i in 1:p) {

    for (j in 1:i) {

      nb=nb+1
      bmat[i,j]=nb

    }
  }
  vb <- vech(bmat)


  # -----------------------
  # final steps
  # -----------------------
  # make identity matrix with dim p
  Imat  = diag(ps)

  permu = Imat[va,vb]

  return(permu)
}
