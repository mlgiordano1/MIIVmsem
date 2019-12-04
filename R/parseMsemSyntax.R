#' Returns 2SLS estimates
#'
#'
#' @param model lavaan style syntax
#'
#' @return list of level 1 and level 2 models
#'
#' @examples parseMSEMSyntax(twolevelmodels)
#'
#' @export
#' @importFrom stringr str_detect fixed str_count str_replace str_locate str_sub
# ------------------------------------------------------------------------------

parseMSEMSyntax <- function(model) {
  # ----------------
  # Setup and checks
  # ----------------

  # check if level: 1 string exists
  if (!str_detect(string  = model,
                           pattern = fixed("level: 1", ignore_case = T))) {
    stop("Did not detect string 'level: 1'")
  }

  # check if level: 2 string exists
  if (!str_detect(string  = model,
                           pattern = fixed("level: 2", ignore_case = T))) {
    stop("Did not detect string 'level: 2'")
  }


  # now check to make sure level: 1 and level: 2 only show up once
  if (str_count(model, fixed("level: 1", ignore_case = T))!=1) {
    stop('"level: 1" string appears more than once')
  }
  if (str_count(model, fixed("level: 2", ignore_case = T))!=1) {
    stop('"level: 2" string appears more than once')
  }



  # -------------------------------------
  # Comment out "level: 1" and "level: 2"
  # -------------------------------------
  model <- str_replace(string      = model,
                                pattern     = fixed("level: 1", ignore_case = T),
                                replacement = "\n# level: 1 \n")
  model <- str_replace(string      = model,
                                pattern     = fixed("level: 2", ignore_case = T),
                                replacement = "\n# level: 2 \n")



  # -----------------------------
  # split up level: 1 and level: 2
  # -----------------------------
  l1_loc <- str_locate(model, "\n# level: 1 \n")
  l2_loc <- str_locate(model, "\n# level: 2 \n")

  # This splits depending on which model is listed first in user syntax
  if (l1_loc[[1,2]] < l2_loc[[1,1]]) {
    l1_model <- str_sub(model, 0, end = l2_loc[1,1]-1)
    l2_model <- str_sub(model, start = l2_loc[1,1], end = nchar(model))
  } else {
    l2_model <- str_sub(model, 0, end = l1_loc[1,1]-1)
    l1_model <- str_sub(model, start = l1_loc[1,1], end = nchar(model))
  }



  return(list(model_level_1 = l1_model, model_level_2 = l2_model))
}

