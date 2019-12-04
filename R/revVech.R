#' This helper function was lifted from MIIVsem
#'
#' @param x tbd
#' @param nrow tbd
#'
#'
#' @return returns a matrix
#'
#' @examples revVech
#'
#' @export
# ------------------------------------------------------------------------------
# For tests, can check the range...can check the average size...can check???


revVech <- function (x, nrow = NULL){
  dim(x) <- NULL
  if (is.null(nrow))
    nrow <- (-1 + sqrt(1 + 8 * length(x)))/2
  output <- matrix(0, nrow, nrow)
  output[lower.tri(output, diag = TRUE)] <- x
  hold <- output
  hold[upper.tri(hold, diag = TRUE)] <- 0
  output <- output + t(hold)
  return(output)
}

