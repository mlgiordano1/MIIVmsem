#' Parse user-supplied instrumental variables
#'
#' @param d tbd
#' @param instruments tbd
#' @param miiv.check tbd
#'
#' @keywords internal
#'
#' @examples parseInstrumentSyntax(d, instruments, miiv.check)
#'
#' @importFrom lavaan lavaanify lavParTable
# ------------------------------------------------------------------------------





parseInstrumentSyntax <- function(d, instruments, miiv.check){

  if (!is.null(instruments) & miiv.check){

    if (class(instruments) != "character"){
      stop("miive: instruments argument must be of class character.")
    }

    if (any(!grepl("~", instruments))) {
       stop("miive: instruments argument not properly specified.")
    }

    mt <- tryCatch(
        {
            lavaanify(instruments)
        },
        error=function(cond) {
             stop("miive: instruments argument not properly specified.")
        },

        finally={
          # do nothing
        }
    )

    mt   <- lavaanify(instruments)
    mt   <- mt[mt$op == "~",]
    mts  <- split(mt[,c("lhs", "rhs")], mt$lhs)

    # Are all the dvs from the instrument list in the eqns list?
    if (length(setdiff(names(mts), lapply(d,"[[","DVobs"))) > 0) {

      stop("miive: dependent variables ",
           paste0(setdiff(
             names(mts),
             lapply(d,"[[","DVobs")),collapse=","
            ),
           " not found in the set of estimating equations.")

    }

    # Are all the MIIVs from the instrument valid MIIVs?
    for (z in names(mts)){

      badMiivs <- setdiff(
        mts[[z]]$rhs,
        d[lapply(d,"[[","DVobs")==z][[1]]$MIIVs
      )

      if (length(badMiivs) > 0){

        stop("MIIVsem: The MIIV(s) ", paste0(badMiivs, collapse =", "),
             " specified for dependent variable ", z,
             " are not valid instruments given the model syntax.")

      } else {
        d[lapply(d,"[[","DVobs")==z][[1]]$MIIVs <- mts[[z]]$rhs
      }
    }

  }

  if (!is.null(instruments) & !miiv.check){

    mt   <- lavParTable(instruments)
    mt   <- mt[mt$op == "~",]
    mts  <- split(mt[,c("lhs", "rhs")], mt$lhs)

    for (z in names(mts)){
      d[lapply(d,"[[","DVobs")==z][[1]]$MIIVs <- mts[[z]]$rhs
    }
  }

  return(d)

}
