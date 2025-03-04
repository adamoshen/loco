check_number_decimal_strict <- function(
  x,
  min,
  max,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if ((x > min) & (x < max)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{.arg {arg}} must be a number strictly between {.num {min}} and {.num {max}}, not {x}.",
    arg = arg,
    call = call
  )
}

check_same_length <- function(
  x,
  y,
  arg1 = rlang::caller_arg(x),
  arg2 = rlang::caller_arg(y),
  call = rlang::caller_env()
) {
  if (length(x) == length(y)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
    "{.arg {arg1}} and {.arg {arg2}} must have the same length.",
    "i" = "{.arg {arg1}} has length {.num {length(x)}}.",
    "i" = "{.arg {arg2}} has length {.num {length(y)}}."
    ),
    arg1 = arg1,
    arg2 = arg2,
    call = call
  )
}

check_complete <- function(x, arg=rlang::caller_arg(x), call=rlang::caller_env()) {
  if (all(!is.na(x))) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{.arg {arg}} cannot contain missing values.",
    arg = arg,
    call = call
  )
}
