get_ac <- function(x, window_size, decay, exp_window) {
  max_timestep <- length(x) - 2 * window_size + 2
  ac <- vector("list", max_timestep)

  ac <- purrr::assign_in(ac, 1, get_first_ac(x, window_size, decay, exp_window))

  if (length(ac) == 1) {
    return(ac)
  }

  for (i in 2:length(ac)) {
    ac <- purrr::assign_in(ac, i, get_next_ac(x, i, ac[[i-1]], window_size, decay, exp_window))
  }

  ac
}

get_first_ac <- function(x, window_size, decay, exp_window) {
  if (exp_window) {
    first_ac <- slider::hop(
      x,
      .starts = seq.int(
        from = 1,
        to = window_size,
        by = 1
      ),
      .stops = seq.int(
        from = window_size,
        to = window_size * 2 - 1,
        by = 1
      ),
      ~ outer(.x, .x)
    )
    first_ac <- purrr::imap(first_ac, ~ .x * decay^(window_size - .y))
  } else {
    first_ac <- slider::hop(
      x,
      .starts = seq.int(
        from = 1,
        to = window_size,
        by = 1
      ),
      .stops = seq.int(
        from = window_size,
        to = window_size * 2 - 1,
        by = 1
      ),
      ~ outer(.x, .x)
    )
  }

  purrr::reduce(first_ac, `+`)
}

get_next_ac <- function(x, index, previous_ac, window_size, decay, exp_window) {
  xt <- x[(window_size + index - 1):(window_size * 2 + index - 1 - 1)]

  if (exp_window) {
    next_ac <- decay * previous_ac + outer(xt, xt)
    return(next_ac)
  }

  xt_lag_w <- x[(index - 1):(window_size + index - 1 - 1)]
  next_ac <- previous_ac - outer(xt_lag_w, xt_lag_w) + outer(xt, xt)
  next_ac
}
