#' Takes a fit obj and returns a dataframe with the parameter estiamtes
#'
#' @param fitObj MSEMMIIVSEM fit obj
#'
#' @return df
#'
#' @examples parameterEstimatesTable(fit)
#'
#' @export
#' @importFrom plyr ldply
# ------------------------------------------------------------------------------


parameterEstimatesTable <- function(fitObj) {
  # this is the list of objects which might need to be repeated on more than one row
  n <- c("est", "se", "newSarg", "EQnum", "EQmod", "DVobs", "IVobs", "DVlat", "IVlat")
  # these might need to be concatenated.
  other <- c("cDist", "MIIVs", "Label")

  # ----------------------
  # Estimates for level 1
  # ----------------------
  ests_within <- fitObj$level1Results
  # n <- names(ests_within[[1]])
  ests_within2 <- c()
  for (i in seq(ests_within)) {
    a <- ests_within[[i]]
    # pulling together the n objects
    b1 <- matrix(nrow = length(a$est), ncol = length(n))
    colnames(b1) <- n
    for (ii in seq(ncol(b1))) {
      b1[,ii] <- a[[n[ii]]]
    }
    # pulling together the 'other' objects
    b2 <- matrix(nrow = length(a$est), ncol = length(other))
    colnames(b2) <- other
    for (ii in seq(ncol(b2))) {
      b2[,ii] <- paste(a[[other[ii]]], collapse = ",")
    }
    ests_within2[[i]] <- cbind.data.frame(b1, b2, stringsAsFactors = FALSE )
  }

  ests_within2 <- ldply(ests_within2, fun = rbind)
  ests_within2["withinBetween"] <- "within"


  # ----------------------
  # Estimates for level 2
  # ----------------------
  ests_between <- fitObj$level2Results
  # n <- names(ests_between[[1]])
  ests_between2 <- c()
  for (i in seq(ests_between)) {
    a <- ests_between[[i]]
    # pulling together the n objects
    b1 <- matrix(nrow = length(a$est), ncol = length(n))
    colnames(b1) <- n
    for (ii in seq(ncol(b1))) {
      b1[,ii] <- a[[n[ii]]]
    }
    # pulling together the 'other' objects
    b2 <- matrix(nrow = length(a$est), ncol = length(other))
    colnames(b2) <- other
    for (ii in seq(ncol(b2))) {
      b2[,ii] <- paste(a[[other[ii]]], collapse = ",")
    }
    ests_between2[[i]] <- cbind.data.frame(b1, b2)
  }

  ests_between2 <- ldply(ests_between2, fun = rbind)
  ests_between2["withinBetween"] <- "between"

  # ----------------------
  #put then together
  # ----------------------
  ests_all <- rbind.data.frame(ests_within2, ests_between2)

  # ----------------------
  # make some numeric
  # ----------------------
  ests_all$est     <- as.numeric(ests_all$est)
  ests_all$se      <- as.numeric(ests_all$se)
  ests_all$newSarg <- as.numeric(ests_all$newSarg)
  ests_all$EQnum   <- as.numeric(ests_all$EQnum)


  ests_all$newSarg.df     <- plyr::laply(strsplit(ests_all$MIIVs,","), length)
  ests_all$newSarg.df     <- ests_all$newSarg.df-1
  ests_all$newSarg.p      <- pchisq(ests_all$newSarg,ests_all$newSarg.df, lower.tail = FALSE)

  return(ests_all)
}
