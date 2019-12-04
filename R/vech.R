#' Takes a square matrix and returns a vector of unique elements
#'
#' @param myMatrix A square matrix
#'
#' @return vector of unique elements
#'
#' @examples vech(df, 2018, 08)
#'
#' @export
# ------------------------------------------------------------------------------

vech <- function(myMatrix) {
  # -------------------------
  # Set up some checks - ex., square vector etc
  # -------------------------
  if (class(myMatrix)!="matrix") {
    stop("input needs to be a square matrix")
  }
  if (ncol(myMatrix)!=nrow(myMatrix)) {
    stop("input needs to be a square matrix")
  }


  if (is.null(rownames(myMatrix))) {
    rownames(myMatrix) <- paste0("V", 1:nrow(myMatrix))
  }
  if (is.null(colnames(myMatrix))) {
    colnames(myMatrix) <- paste0("V", 1:ncol(myMatrix))
  }


  # generic counter
  ii = 0
  # save number of rows in A
  p = nrow(myMatrix)
  # number of unique elements
  pstar = p*(p+1)/2
  # empty vector
  Va = matrix(nrow = pstar, ncol = 1)
  rownames(Va) <- 1:pstar
  # looping rows and columns of matrix to fill vector
  for (cc in 1:p) {
    for (rr in cc:p) {
      ii = ii+1
      Va[ii] <- myMatrix[rr, cc]
      rownames(Va)[ii] <- paste0(rownames(myMatrix)[rr], "...", colnames(myMatrix)[[cc]])
    }
  }

  return(Va)
}




