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
