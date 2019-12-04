#' Takes a list of observed variables and makes writes the lavaan syntax for a
#' fully saturated lavaan model
#'
#' @param obsVars a vector of names of the observed vars in a model
#'
#' @return lav model syntax for saturated two level model
#'
#' @examples writeSatLavModel(obsVars)
#'
#' @export
# ------------------------------------------------------------------------------
writeSatLavModel <- function(obsVars) {


  # --------------------------------------
  # makes the general saturated expression
  # --------------------------------------
  l <- length(obsVars)
  li <- vector("list", l)
  for(i in seq(l)) {
    li[[i]] <- paste(obsVars[i], "~~", paste(obsVars[i:l], collapse = "+"))
  }
  li <- paste(li, collapse = "\n")




  # ----------------------------------------------------
  # take the general sat model and write for both levels
  # ----------------------------------------------------
  fullMod <- paste0("Level: 1 \n", li, "\n\nLevel: 2 \n", li)




  return(fullMod)
}





