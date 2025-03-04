check_column_exists <- function(
    df,
    x,
    allow_null = FALSE,
    arg_df = rlang::caller_arg(df),
    arg_x = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  x <- rlang::enquo(x)

  if(rlang::quo_is_null(x) & allow_null) {
    return(invisible(NULL))
  }
  if (rlang::as_name(x) %in% names(df)) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "{.arg {arg_x}} must be a column in {.arg {arg_df}}.",
    arg_df = arg_df,
    arg_x = arg_x,
    call = call
  )
}

check_number_decimal_strict <- function(
  x,
  min,
  max,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_number_decimal(x)

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
