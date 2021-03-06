

cli_glue = function(..., .envir = parent.frame()) {

  txt = cli::cli_format_method(cli::cli_text(..., .envir = .envir))

  # cli_format_method does wrapping which we dont want at this stage
  # so glue things back together.
  paste(txt, collapse = " ")
}

cli_stop = function(..., .envir = parent.frame()) {
  text = cli_glue(..., .envir = .envir)
  stop(paste(text, collapse = "\n"), call. = FALSE)
}

cli_warn = function(..., .envir = parent.frame()) {
  text = cli_glue(..., .envir = .envir)
  warning(paste(text, collapse = "\n"), call. = FALSE)
}
