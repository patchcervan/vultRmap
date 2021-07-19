#' Calculate utilization distribution at rotor height around a colony
#'
#' @param .col_sel A named vector (data frame row) with at least three elements:
#' c("lon", "lat", "id", "avg_ad", "avg_juv")
#' @param .age Age of the vultures a utilization distribution is calculated for.
#' @param .hab A dataframe with the longitude ("lon") latitude ("lat") of the
#' grid cells the UD is computed at. If NULL, the object "range_covts" will be
#' used.
#' @param .scale TRUE/FALSE whether the UD should be scaled by the number of
#' individuals expected to use the colony.
#' @param .countsdir A character string with the directory where the smoothed
#' counts(expected counts, in this case) are stored.
#' @param .suffix A character string containing the part the name of the file
#' that appears after the colony code (e.g. "_ad_hgt_gam.rds)
#' @param .outputdir A character string with the directory to store the results.
#' If NULL (default) the output is not saved to disk.
#'
#' @return A dataframe with the expected counts and smoothed expected counts
#' computed for each grid cell in .hab.
#' @export
#'
#' @examples
calcUdHgtColony <- function(.col_sel, .age, .hab = NULL, .scale = FALSE,
                            .countsdir, .suffix, .outputdir = NULL){

  if(is.null(.hab)){
    # Load habitat grid
    .hab <- vultRmap::range_covts
    attr(.hab, "mod_scale") <- NULL
  }

  .hab <- .hab %>%
    dplyr::mutate(lon = round(lon, 3),
                  lat = round(lat, 3))

  # Load counts/GAM dataframe
  ud_col <- readRDS(paste0(.countsdir, .col_sel$id, .suffix))

  ud_col <- ud_col %>%
    dplyr::mutate(lon = round(lon, 3),
                  lat = round(lat, 3))

  # Calculate total count
  total_count <- sum(ud_col$count, na.rm = T)
  total_gamfit <- sum(ud_col$gamfit, na.rm = T)

  # Join counts and grid cells
  .hab <- .hab %>%
    dplyr::select(lon, lat) %>%
    dplyr::left_join(ud_col, by = c("lon", "lat"))

  # Those grid cells not in counts should be zero
  .hab$count[is.na(.hab$count)] <- 0
  .hab$gamfit[is.na(.hab$gamfit)] <- 0

  # Standardize counts
  .hab$count <- .hab$count/total_count
  .hab$gamfit <- .hab$gamfit/total_gamfit

  # Create temporary name for output file
  udfile <- paste0(.outputdir, .col_sel$id, "_", .age, "_hgt_hazard.rds")

  # Scale if necessary
  if(.scale){

    # Define size of colony
    size <- dplyr::case_when(.age == "ad" ~ as.numeric(.col_sel$avg_ad),
                             .age == "juv" ~ as.numeric(.col_sel$avg_juv))

    # Scale standardized counts by size of the colony
    .hab$count <- .hab$count*size
    .hab$gamfit <- .hab$gamfit*size

    # Change name of output file if necessary
    udfile <- paste0(.outputdir, .col_sel$id, "_", .age, "_hgt_risk.rds")

  }

  if(!is.null(.outputdir)){
    saveRDS(.hab, udfile)
  }

  return(.hab)

}
