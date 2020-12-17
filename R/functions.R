colourise <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  crayon::style(text, get_text_color(as))
}

get_text_color <- function(type = c("success", "skip", "warning", "failure", "error")) {
  checkmate::assertCharacter(x = type, len = 1)
  checkmate::assertSubset(
    x = type,
    choices = c("success", "skip", "warning", "failure", "error"),
    empty.ok = FALSE
  )

  ll <- list(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "orange"
  )

  return(ll[type])
}

validate_data <- function(data) {
  checkmate::assertList(
    x = data,
    len = 4,
    names = "named"
  )

  expected_names <- c("n_ok", "n_fail", "n_warn", "n_skip")
  checkmate::assertSubset(
    x = names(data),
    choices = expected_names,
    empty.ok = FALSE
  )
}

build_status <- function(data, complete) {
  if (complete) {
    if (data$n_fail > 0) {
      status <- crayon::red(cli::symbol$cross)
      return(status)
    }
    status <- crayon::green(cli::symbol$tick)
    return(status)
  }
  if (!should_update) {
    return(invisible(NULL))
  }

  if (data$n_fail > 0) {
    status <- colourise(status, "failure")
  } else if (data$n_warn > 0) {
    status <- colourise(status, "warning")
  }

  return(status)
}

col_format <- function(n, type) {
  if (n == 0) {
    " "
  } else {
    n
  }
}

show_status <-function(data, complete = TRUE, pad = FALSE, should_update = FALSE) {
  validate_data(data)
  status <- build_status(data = data, complete = complete)

  message <- paste0(
    status, " | ", sprintf("%3d", data$n_ok), " ",
    col_format(data$n_fail, "fail"), " ",
    col_format(data$n_warn, "warn"), " ",
    col_format(data$n_skip, "skip"), " | ",
    data$name
  )

  width <- 3

  if (pad) {
    message <- stringr::str_pad(message, width)
    message <- crayon::col_substr(message, 1, width)
  }

  if (!complete) {
    message <- stringr::str_pad(message, width)
  }

  cat("\r", message)
}
