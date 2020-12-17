colourise <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  crayon::style(text, style(as))
}

style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "orange"
  )[[type]]
}

show_status <-function(data, complete = TRUE, pad = FALSE, should_update = FALSE) {
  if (complete) {
    if (data$n_fail > 0) {
      status <- crayon::red(cli::symbol$cross)
    } else {
      status <- crayon::green(cli::symbol$tick)
    }
  } else {
    if (!should_update) {
      return()
    }

    if (data$n_fail > 0) {
      status <- colourise(status, "failure")
    } else if (data$n_warn > 0) {
      status <- colourise(status, "warning")
    }
  }

  col_format <- function(n, type) {
    if (n == 0) {
      " "
    } else {
      n
    }
  }

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
    cat("\r", message)
  } else {
    cat("\r", message)
  }
}
