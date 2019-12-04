#' takes an argument 'p' and returns a duplication matrix as defined in Magnus&Neudecker;
#' also see matrixcalc::duplication.matrix() for a possibly faster implementation
#'
#' @param p any scalar
#'
#' @return matrix. duplication matrix of 1's and 0's
#'
#' @examples yb2007_DP(6)
#'
#' @export
# ------------------------------------------------------------------------------


yb2007_DP <- function(p) {
  # create the output matrix, filling all entries as 0
  Dup <- matrix(nrow=p*p,ncol=p*(p+1)/2, 0)

  # linear counter
  count=0

  for (j in 1:p) {

    for (i in j:p) {

      count=count+1;

      if (i==j) {

        Dup[(j-1)*p+j, count] = 1;

      } else {

        Dup[(j-1)*p+i, count] = 1;
        Dup[(i-1)*p+j, count] = 1;

      }
    }
  }

  return(Dup)
}





