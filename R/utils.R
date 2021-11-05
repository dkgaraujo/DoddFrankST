find_file <- function(file_url, cache_file = cache_file, cache_folder_name = "cache") {
  file_name <- strsplit(file_url, "/", fixed = TRUE)
  file_name <- file_name[[1]][length(file_name[[1]])]
  cached_file_name <- file.path(cache_folder_name, file_name)
  if (file.exists(cached_file_name)) {
    file_path <- cached_file_name
  } else {
    if (cache_file) {
      if (!dir.exists(cache_folder_name)) {
        dir.create(cache_folder_name)
      }
      try(
        utils::download.file(file_url, cached_file_name)
      )
      if (file.exists(cached_file_name)) {
        file_path <- cached_file_name
      }
    } else {
      file_path <- file_url
    }
  }
  return(file_path)
}

consistent_sample <- function(banklevel_results, min_year = NULL) {
  if (!is.null(min_year)) {
    banklevel_results <- banklevel_results %>%
      dplyr::filter(format(dt_exercise_quarter, "%Y") >= min_year)
  }

  bank_sample <- banklevel_results %>%
    dplyr::select(id_rssd, dt_exercise_quarter) %>%
    tidyr::pivot_wider(names_from = dt_exercise_quarter,
                       values_from = dt_exercise_quarter,
                       values_fn = length) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::select(id_rssd)

  banklevel_results <- banklevel_results %>%
    filter(id_rssd %in% bank_sample$id_rssd)

  return(banklevel_results)
}
