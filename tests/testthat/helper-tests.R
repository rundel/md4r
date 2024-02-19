if (Sys.getenv("CI") == "") {
  exprs <- gfm_tests_to_html()
  text <- purrr::map(exprs, constructive::deparse_call)
  flat <- purrr::reduce(text, ~ c(.x, "", .y))
  writeLines(flat, "test-to_html-gfm.R")
}
