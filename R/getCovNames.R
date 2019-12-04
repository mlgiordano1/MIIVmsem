#' This function is just to help name parts of the acov and the vectorized components
#'
#' @param varList vector of names of the variables (in order!)
#'
#' @return list of names for rows and names for columns
#'
#' @examples getCovNames(x)
#'
#' @export
# ------------------------------------------------------------------------------

getCovNames <- function(varList) {
  p <- length(varList)
  namesForRows <- vector(mode = "character", length = (p*(p+1)/2))
  namesForCols <- vector(mode = "character", length = (p*(p+1)/2))
  # looping rows and columns of matrix to fill vector
  ii=1
  for (cc in 1:p) {
    for (rr in cc:p) {
      namesForRows[ii] <- paste0(varList[[rr]], "~~", varList[[cc]])
      ii = ii+1
    }
  }
  # now for column names (just reverse the cc/rr)
  ii=1
  for (rr in 1:p) {
    for (cc in rr:p) {
      namesForCols[ii] <- paste0(varList[[rr]], "~~", varList[[cc]])
      ii = ii+1
    }
  }

  return(list(namesForRows = namesForRows, namesForCols = namesForCols))
}


