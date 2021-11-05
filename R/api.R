#' Access the results of Dodd-Frank Act Stress Tests
#'
#' @param banklevel_only Boolean. If TRUE, aggregate results are filtered out of the dataset.
#' @param consistent_sample_from_year Either NULL or a four-digit year starting with 2013. If a year is provided, the consistent sample (ie, banks with information for all years including and after that) is returned.
#' @param cache_file Boolean. If TRUE, the raw file is downloaded to a local folder and used to load the information in subsequent runs.
#' @return A tidy tibble with the stress tests results.
#' @export
bank_results <- function(banklevel_only = TRUE, consistent_sample_from_year = NULL, cache_file = TRUE) {
  file_path <- find_file(file_url = url_banklevel, cache_file = cache_file)
  results <- readr::read_csv(file_path)
  results <- dplyr::mutate(results, dt_exercise_quarter = as.Date(dt_exercise_quarter, "%m/%d/%Y"))

  if (banklevel_only) {
    results <- results %>%
      dplyr::filter(!is.na(id_rssd))
  }

  if (!is.null(consistent_sample_from_year)) {
    if (!banklevel_only) {
      print("Obs.: `consistent_sample` set to `TRUE` always returns bank-level results only, discarding the aggregate results.")
      banklevel_only = TRUE
    }
    results <- results %>%
      dplyr::filter(!is.na(id_rssd)) %>%
      consistent_sample(min_year = consistent_sample_from_year)
  }
  return(results)
}

