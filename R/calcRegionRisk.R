#' Calculate region risk
#'
#' @param region An \link[sf]{sf} polygon or multipolygon object to predict risk
#' for. Risk is given as number of individuals at risk.
#' @param ref_area Optional. An \link[sf]{sf} polygon object. if provided risk
#' is calculated relative to this reference area as a the proportion of
#' individuals at risk.
#' @param risk_map A \link[raster]{raster} object with from which risk values
#' are extracted from.
#'
#' @return
#' @export
#' @importFrom furrr future_map_dbl
#' @examples
calcRegionRisk <- function(region, ref_area = NULL, risk_map){

  region_risk <- exactextractr::exact_extract(risk_map, region)

  region_risk <- furrr::future_map(region_risk, ~dplyr::filter(.x, coverage_fraction > 0.5))

  if(is.null(ref_area)){

    region <- region %>%
      dplyr::mutate(risk_total = future_map_dbl(region_risk, ~sum(.x$value, na.rm = TRUE)),
                    risk_avg = future_map_dbl(region_risk, ~mean(.x$value, na.rm = TRUE)),
                    risk_sd = future_map_dbl(region_risk, ~sd(.x$value, na.rm = TRUE)))

    attr(region, "ref_risk") <- NULL

  } else {

    total <- sum(exactextractr::exact_extract(risk_map, ref_area, fun = "sum"))

    region <- region %>%
      dplyr::mutate(risk_total = future_map_dbl(region_risk, ~sum(.x$value, na.rm = TRUE)),
                    risk_avg = future_map_dbl(region_risk, ~mean(.x$value, na.rm = TRUE)),
                    risk_sd = future_map_dbl(region_risk, ~sd(.x$value, na.rm = TRUE)),
                    risk_prop = risk_total / total)

    attr(region, "ref_risk") <- total

  }

  # order fields
  region <- region %>%
    dplyr::relocate(geometry, .after = last_col())

  return(region)


}
