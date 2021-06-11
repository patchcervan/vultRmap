#' Complete data frame
#'
#' @description Takes a data frame and adds a number of missing variables filling them with NA or other value provided
#' @param df Data frame with missing variables
#' @param allvars A character vector with the names of all the variables the data frame should have
#' @param value The value that should have the missing variables by default
#'
#' @return A data frame with all the variables contained in 'df' and 'allvars'. Those variables originally contained in 'allvars' and not in 'df' will be added to 'df' and given a value
#' specified in the 'value' field. One would tipically fill in those variable with some desired values.
#' @export
#'
#' @examples
#' completeDataFrame(range_covts, c("disturbed", "dist_col:ttnoon"), 0)
completeDataFrame <- function(df, allvars, value){

  # Keep attributes other than the regular data frame ones
  newatb <- attributes(df)[!names(attributes(df)) %in% c("names", "class", "row.names")]

  # Subset those that are missing
  missvars <- allvars[!allvars %in% names(df)]

  # Create a list of new variables and add to the original data frame
  newvars <- vector("list", length = length(missvars))
  for(i in seq_along(newvars)){
    newvars[[i]] <- rep(value, nrow(df))
  }
  names(newvars) <- missvars

  df <- df %>%
    cbind(do.call("cbind", newvars))

  # Add attributes
  for(i in seq_along(newatb)){
    attr(df, names(newatb)[[i]]) <- newatb[[i]]
  }

  return(df)

}
