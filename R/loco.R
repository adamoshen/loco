loco <- function(
    .data,
    x,
    y,
    timestamps = NULL,
    demean = TRUE,
    window_size,
    decay = 0.95,
    exp_window = TRUE,
    k
) {
  check_data_frame(.data)
  check_symbol(x)
  check_symbol(y)
  check_symbol(timestamps, allow_null=TRUE)
  check_bool(demean)
  check_number_decimal_strict(decay, min=0, max=1)
  check_bool(exp_window)

  x <- dplyr::pull(.data, {{ x }})
  y <- dplyr::pull(.data, {{ y }})

  check_same_length(x, y)
  check_number_whole(window_size, min=1, max=floor(length(x) / 2))
  check_number_whole(k, min=1, max=window_size)
  check_complete(x)
  check_complete(y)

  if (demean) {
    x <- x - mean(x)
    y <- y - mean(y)
  }

  ac_x <- get_ac(x, window_size, decay, exp_window)
  ac_y <- get_ac(y, window_size, decay, exp_window)

  eigen_x <- get_ac_principal_evectors(ac_x, k)
  eigen_y <- get_ac_principal_evectors(ac_y, k)

  scores <- purrr::map2_dbl(eigen_x, eigen_y, ~ get_loco_score(.x, .y))

  if (rlang::quo_is_null(rlang::enquo(timestamp))) {
    return(tibble::as_tibble_col(scores, "scores"))
  }

  timestamps <- dplyr::pull(.data, {{ timestamps }})
  timestamps <- timestamps[
    -c(1:(window_size - 1), (length(timestamps) - window_size + 2):length(timestamps))
  ]

  tibble::tibble(timestamps, scores)
}

get_ac_principal_evectors <- function(ac_list, k) {
  principal_evectors <- purrr::map(ac_list, ~ eigen(.x, symmetric=TRUE))
  principal_evectors <- purrr::map(principal_evectors, "vectors")
  principal_evectors <- purrr::modify(principal_evectors, ~ reduce_eigen_dimensions(.x, k))
  principal_evectors
}

reduce_eigen_dimensions <- function(e, k) {
  e[, 1:k, drop=FALSE]
}

get_loco_score <- function(xvectors, yvectors) {
  0.5 * (
    norm(t(xvectors) %*% principal_evector(yvectors), type="2") +
      norm(t(yvectors) %*% principal_evector(xvectors), type="2")
  )
}

principal_evector <- function(e) {
  e[, 1, drop=FALSE]
}
