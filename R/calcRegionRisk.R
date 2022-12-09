#' Calculate population exposed at a region
#'
#' @param region An \link[sf]{sf} polygon or multipolygon object to predict exposure
#' for. Exposure is given as number of individuals potentially found in the region at any given time.
#' @param ref_area Optional. An \link[sf]{sf} polygon object. if provided exposure
#' is calculated relative to this reference area as the proportion of
#' individuals exposed in the region relative to the total found in the reference area.
#' @param ud_map A \link[raster]{raster} object with from which population
#' utilization distribution values are extracted from.
#'
#' @return
#' @export
#' @importFrom furrr future_map_dbl
#' @examples
calcRegionRisk <- function(region, ref_area = NULL, ud_map){

  region_pud <- exactextractr::exact_extract(ud_map, region)

  region_pud <- furrr::future_map(region_pud, ~dplyr::filter(.x, coverage_fraction > 0.5))

  if(is.null(ref_area)){

    region <- region %>%
      dplyr::mutate(pud_total = future_map_dbl(region_pud, ~sum(.x$value, na.rm = TRUE)),
                    pud_avg = future_map_dbl(region_pud, ~mean(.x$value, na.rm = TRUE)),
                    pud_sd = future_map_dbl(region_pud, ~sd(.x$value, na.rm = TRUE)))

    attr(region, "ref_population") <- NULL

  } else {

    total <- sum(exactextractr::exact_extract(ud_map, ref_area, fun = "sum"))

    region <- region %>%
      dplyr::mutate(pud_total = future_map_dbl(region_pud, ~sum(.x$value, na.rm = TRUE)),
                    pud_avg = future_map_dbl(region_pud, ~mean(.x$value, na.rm = TRUE)),
                    pud_sd = future_map_dbl(region_pud, ~sd(.x$value, na.rm = TRUE)),
                    pud_prop = pud_total / total)

    attr(region, "ref_population") <- total

  }

  # order fields
  region <- region %>%
    dplyr::relocate(geometry, .after = last_col())

  return(region)


}
