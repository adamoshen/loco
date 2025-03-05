#' Calculate local correlation scores
#'
#' Calculate local correlation (LoCo) scores for a pair of time series.
#'
#' The LoCo score is an extension of the linear cross-correlation coefficient and measures the local
#' similarity (concurrent alignment) between two time series using the spectral decompositions of
#' local autocovariance matrices. The LoCo score is a value between 0 (weak alignment) and 1
#' (strong alignment).
#'
#' @param .data A data frame or a data frame extension (e.g. a tibble).
#' @param x,y Names of the columns containing the time series data, as symbols.
#' @param timestamps An optional parameter specifying the name of the column, as a symbol,
#' containing timestamps corresponding to `x` and `y`.
#' @param demean Subtract the series' respective means from the time series values. Defaults to TRUE.
#' @param window_size The size of the window to use when calculating autocovariances.
#' @param decay The exponential decay factor used to downweight historic time windows when using an
#' exponential sliding window. Should be a value strictly between 0 (most decay) and 1
#' (least decay). Ignored if `exp_window` is FALSE.
#' @param exp_window Should an exponential sliding window be used? Defaults to TRUE. If FALSE, a
#' sliding (a.k.a. boxcar) window is used.
#' @param k The number of principal eigenvectors to consider when calculating the LoCo scores.
#' Value should be an integer between 1 and `window_size`.
#' @return A [tibble][tibble::tibble-package] with columns `timestamps` (if supplied) and `scores`.
#' @references Papadimitriou, S., Sun, J., and Yu, P.S. (2006) *Local Correlation Tracking in Time
#' Series*. Proceedings of the Sixth International Conference on Data Mining (ICDM'06).
#' [doi:10.1109/ICDM.2006.99](https://doi.org/10.1109/ICDM.2006.99).
#' @export
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
  check_column_exists(.data, {{ x }})
  check_column_exists(.data, {{ y }})
  check_column_exists(.data, {{ timestamps }}, allow_null=TRUE)
  check_bool(demean)
  check_number_decimal_strict(decay, min=0, max=1)
  check_bool(exp_window)

  x <- dplyr::pull(.data, {{ x }})
  y <- dplyr::pull(.data, {{ y }})

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

  if (rlang::quo_is_null(rlang::enquo(timestamps))) {
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
