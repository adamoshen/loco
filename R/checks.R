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
  arg_x = rlang::caller_arg(x),
  arg_y = rlang::caller_arg(y),
  call = rlang::caller_env()
) {
  if (length(x) == length(y)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
    "{.arg {arg_x}} and {.arg {arg_y}} must have the same length.",
    "*" = "{.arg {arg_x}} has length {.num {length(x)}}.",
    "*" = "{.arg {arg_y}} has length {.num {length(y)}}."
    ),
    arg_x = arg_x,
    arg_y = arg_y,
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
