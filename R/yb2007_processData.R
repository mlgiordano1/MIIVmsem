#' Subroutine that process the data
#'
#' @param ymat    description tbd
#' @param p       description tbd
#' @param nlevel1 description tbd
#' @param NG      description tbd
#'
#' @return list
#'
#' @examples yb2007_processData()
#'
#' @export
# ------------------------------------------------------------------------------


yb2007_processData <- function(ymat, p, nlevel1, NG) {

  # ----------------------------------------------------------------------------------------
  # I feel like this makes the erroneous assumption that groups are numbered sequentially...
  # might need to do some changing for this.
  # ----------------------------------------------------------------------------------------

  # total sum (also nrow(ymat)?)
  nt      <- sum(nlevel1[,"n"])
  # unique cov elements
  ps      <- p*(p+1)/2;

  # setting up some matrices
  ymean   <- matrix(nrow = NG, ncol = p,  0) # this matrix holds all the group means
  vsmatL1 <- matrix(nrow = NG, ncol = ps, 0) #
  smatw   <- matrix(nrow = p,  ncol = p,  0)

  nsum=0

  for (jj in 1:NG) {

    # pull the name and size of single group
    nj    <- nlevel1[jj,]

    # subset ymat by the rows of the current group
    ymatj <- ymat[(nsum+1):(nsum+nj["n"]),]

    # advance the counter
    nsum  <- nsum+nj["n"]

    # saving the group means for group jj (I wonder if I could use dplyr to do the group means in one action)
    ymean[jj,] <- matrix(nrow = 1, ncol = nj["n"], 1) %*% ymatj/nj["n"]


    # ---------------------------------
    # Need to ID these last few steps
    # they work I just don't know their purpose yet...
    # ---------------------------------
    njsmatj      <- t(ymatj) %*% (diag(nj["n"])-matrix(nrow = nj["n"], ncol = nj["n"],1)/nj["n"]) %*% ymatj
    smatw        <- smatw+njsmatj
    vsmatj       <- vech(njsmatj)
    vsmatL1[jj,] <- t(vsmatj)


  }

  smatw <- smatw/(nt-NG)


  return(list(smatw   = smatw,
              ymean   = ymean,
              vsmatL1 = vsmatL1))
}
